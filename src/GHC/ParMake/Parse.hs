-- Parsing.

module GHC.ParMake.Parse (getModuleDeps)
       where

import Data.Char (isAlphaNum, isSpace)
import Data.Functor ((<$>))
import Data.Maybe (catMaybes)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (openFile, hGetContents, IOMode(..))
import System.IO.Temp (withSystemTempDirectory)

import Distribution.Compat.ReadP
import GHC.ParMake.Util (defaultOutputHooks, runProcess)

parseModuleName :: ReadP r String
parseModuleName = munch1 (\c -> isAlphaNum c || c == '.'
                                || c == '-'  || c == '/')

parseLine :: String -> Maybe (String, String)
parseLine l = case [ r | (r,rest) <- readP_to_S parser l, all isSpace rest] of
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
getModuleDeps :: FilePath -> [String] -> [FilePath] -> IO [(String, String)]
getModuleDeps ghcPath ghcArgs files =
  withSystemTempDirectory "ghc-parmake" $ \tmpDir -> do
    let tmpFile = tmpDir </> "depends.mk"
    let ghcArgs' = files ++ ("-M":"-dep-makefile":tmpFile:ghcArgs)
    exitCode <- runProcess defaultOutputHooks Nothing ghcPath ghcArgs'
    if exitCode == ExitSuccess
      then (catMaybes . map parseLine . trimLines . lines) <$>
           (openFile tmpFile ReadMode >>= hGetContents)
      else return []