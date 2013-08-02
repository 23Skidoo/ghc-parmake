{-# LANGUAGE TupleSections #-}

-- Parsing.

module GHC.ParMake.Parse (Dep (..), getModuleDeps)
       where

import Control.Concurrent
import Control.Monad
import Data.Char (isAlphaNum, isSpace)
import Data.Functor ((<$>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Distribution.Compat.ReadP
import GHC.ParMake.Util (Dep(..), Verbosity, debug', defaultOutputHooks, runProcess)


-- TODO This random choice of characters is *insane*, this will NOT WORK when
--      some unexpected character is in the filename.
--      Worse even, `parseLine` will just return Nothing, silencing the
--      problem and making ghc-parmake exit with code 1 without reason.
--
--      This filename parsing and "careful" parsing (returning Nothing by
--      default instead of erroring) must be changed!
parseModuleName :: ReadP r String
parseModuleName = munch1 (\c -> isAlphaNum c || c == '.'
                                || c == '-'  || c == '/' || c == '_')

parseLine :: String -> Maybe (String, String)
parseLine l = case [ r | (r, rest) <- readP_to_S parser l, all isSpace rest] of
  []  -> Nothing
  [r] -> Just r
  _   -> Nothing
  where
    parser = do skipSpaces
                m <- parseModuleName
                skipSpaces
                _ <- char ':'
                skipSpaces
                d <- parseModuleName
                skipSpaces
                return (m,d)

trimLines :: [String] -> [String]
trimLines ls = [ l | l <- ls, isValidLine l]
  where
    isValidLine ('#':_) = False
    isValidLine _       = True

-- Interaction with the outside world.

-- Run 'ghc -M' and return dependencies for every module.
getModuleDeps :: Verbosity
              -> FilePath
              -> [String]
              -> [FilePath]
              -> IO [Dep]
getModuleDeps v ghcPath ghcArgs files =
  withSystemTempDirectory "ghc-parmake" $ \tmpDir -> do

    let tmpFileInternal = tmpDir </> "depends.internal.mk"
        tmpFileExternal = tmpDir </> "depends.external.mk"

    let ghcArgsInternal = files ++ ("-M":"-dep-makefile":tmpFileInternal:ghcArgs)
        ghcArgsExternal = files ++ ("-M":"-dep-makefile":tmpFileExternal:"-include-pkg-deps":ghcArgs)

    -- Get all dependencies in this package
    let getInternalMakeDeps = do
          debug' v $ "Running compiler with -M to get internal module deps: "
                     ++ ghcPath ++ " " ++ show ghcArgsInternal
          failOnError <$> runProcess defaultOutputHooks Nothing ghcPath ghcArgsInternal
          parseDepsFromFile tmpFileInternal

    -- Pass -include-pkg-deps to also depend on external dependencies
    let getExternalMakeDeps = do
          debug' v $ "Running compiler with -M to get external module deps: "
                     ++ ghcPath ++ " " ++ show ghcArgsExternal
          failOnError <$> runProcess defaultOutputHooks Nothing ghcPath ghcArgsExternal
          parseDepsFromFile tmpFileExternal

    -- The two ghc -M are mainly CPU-bound. Run them in parallel.
    [internalMakeDeps, externalMakeDeps] <- parallelIO [ getInternalMakeDeps
                                                       , getExternalMakeDeps ]

    -- Put internal and external deps together
    let depsIntExt = mergeValues (groupByTarget internalMakeDeps)
                                 (groupByTarget externalMakeDeps)

    return [ Dep target int ext | (target, (int, ext)) <- Map.toList depsIntExt ]
  where
    failOnError ExitSuccess     = ()
    failOnError (ExitFailure n) = error $ "ghc-parmake: ghc -M exited with status "
                                          ++ show n

    parseDepsFromFile :: FilePath -> IO [(String, String)]
    parseDepsFromFile file = catMaybes . map parseLine . trimLines . lines
                             <$> readFile file

-- * Helpers

-- | Runs the IO actions in parallel, and waits until all are finished.
parallelIO :: [IO a] -> IO [a]
parallelIO ios = do
  mvars <- forM ios $ \io -> do m <- newEmptyMVar
                                _ <- forkIO $ io >>= putMVar m
                                return m
  mapM readMVar mvars

-- | Groups a list of (targets, dependencies) by the targets.
groupByTarget :: (Ord target) => [(target, dep)] -> Map target [dep]
groupByTarget deps = Map.fromListWith (++) [ (t, [d]) | (t, d) <- deps ]

-- | Merges two maps that have the same keys.
mergeValues :: (Ord k) => Map k [a] -> Map k [b] -> Map k ([a], [b])
mergeValues m1 m2 = Map.mergeWithKey (\_ as bs -> Just (as, bs)) (fmap (, [])) (fmap ([], )) m1 m2
