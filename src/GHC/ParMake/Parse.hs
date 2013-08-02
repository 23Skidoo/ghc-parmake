-- Parsing.

module GHC.ParMake.Parse (getModuleDeps)
       where

import Data.Char (isAlphaNum, isSpace)
import Data.Functor ((<$>))
import Data.Maybe (catMaybes)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Distribution.Compat.ReadP
import GHC.ParMake.Util (Verbosity, debug', defaultOutputHooks, runProcess)


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
              -> IO [(String, String)]
getModuleDeps v ghcPath ghcArgs files =
  withSystemTempDirectory "ghc-parmake" $ \tmpDir -> do
    let tmpFile = tmpDir </> "depends.mk"
    let ghcArgs' = files ++ ("-M":"-dep-makefile":tmpFile:ghcArgs)
    debug' v $ "Running compiler with -M to get module deps: "
               ++ ghcPath ++ " " ++ show ghcArgs'
    exitCode <- runProcess defaultOutputHooks Nothing ghcPath ghcArgs'
    if exitCode == ExitSuccess
      then (catMaybes . map parseLine . trimLines . lines) <$>
           (readFile tmpFile)
      else exitFailure
