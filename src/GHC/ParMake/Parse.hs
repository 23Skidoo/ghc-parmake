{-# LANGUAGE TupleSections #-}

-- Parsing.

module GHC.ParMake.Parse (getModuleDeps, depsListToDeps)
       where

import Control.Concurrent
import Control.Monad
import Data.Char (isAlphaNum, isSpace)
import Data.Functor ((<$>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Distribution.Compat.ReadP
import GHC.ParMake.Types (Dep(..))
import GHC.ParMake.Util (Verbosity, debug', fatal,
                         defaultOutputHooks, runProcess)


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
        ghcArgsExternal = files ++
            ("-M":"-dep-makefile":tmpFileExternal:"-include-pkg-deps":ghcArgs)

    -- Get all internal dependencies in this package.
    let getInternalMakeDeps = do
          debug' v $ "Running compiler with -M to get internal module deps: "
                     ++ ghcPath ++ " " ++ show ghcArgsInternal
          failOnError <$> runProcess defaultOutputHooks Nothing
                                     ghcPath ghcArgsInternal
          parseDepsFromFile tmpFileInternal

    -- Pass -include-pkg-deps to also find out the external dependencies.
    let getAllMakeDeps = do
          debug' v $ "Running compiler with '-M -include-pkg-deps' "
            ++ "to get external module deps: "
            ++ ghcPath ++ " " ++ show ghcArgsExternal
          failOnError <$> runProcess defaultOutputHooks Nothing
                                     ghcPath ghcArgsExternal
          parseDepsFromFile tmpFileExternal

    -- The two ghc -M are mainly CPU-bound. Run them in parallel.
    [internalMakeDeps, allMakeDeps] <- parallelIO [ getInternalMakeDeps
                                                  , getAllMakeDeps ]

    -- Put internal and internal + external deps together
    let depsIntAll = mergeValues (groupByTarget internalMakeDeps)
                                 (groupByTarget allMakeDeps)

    -- External deps are (all - internal) ones.
    return [ Dep target int (intExt `diff` int)
           | (target, (int, intExt)) <- Map.toList depsIntAll ]
  where
    failOnError (ExitSuccess  ) = ()
    failOnError (ExitFailure n) =
      fatal $ "ghc -M exited with status " ++ show n

    parseDepsFromFile :: FilePath -> IO [(String, String)]
    parseDepsFromFile file = catMaybes . map parseLine . trimLines . lines
                             <$> readFile file

-- * Helpers

-- | Fast list difference. Uses `Set.difference`, but preserves order.
diff :: (Ord a) => [a] -> [a] -> [a]
xs `diff` ys = filter (`Set.member` diffSet) xs
  where
    diffSet = Set.fromList xs `Set.difference` Set.fromList ys

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
mergeValues m1 m2 = Map.unionWith (\(a,b) (x,y) -> (a ++ x, b ++ y))
                                  (fmap (, []) m1)
                                  (fmap ([], ) m2)

-- | Converts a list of (targets, dependencies) to a `Dep` list
-- with no external dependencies.
depsListToDeps :: [(FilePath, FilePath)] -> [Dep]
depsListToDeps l = [ Dep t ds [] | (t, ds) <- Map.toList (groupByTarget l) ]
