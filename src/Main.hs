module Main
       where

import Distribution.Compat.ReadP

import Control.Concurrent
import Control.Monad (forever, forM_)
import qualified Control.Exception as Exception
import Data.Char (isAlphaNum, isSpace)
import Data.Functor ((<$>))
import Data.List (groupBy)
import Data.Maybe (catMaybes, listToMaybe)
import System.Environment (getArgs)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (openFile, hClose, hGetContents, hPutStrLn,
                  hSetBinaryMode, stderr, IOMode(..))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (runInteractiveProcess, waitForProcess)

-- Helpers.

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

pathToMaybe :: FilePath -> Maybe FilePath
pathToMaybe [] = Nothing
pathToMaybe p  = Just p

-- Process creation.

-- Taken from Distribution.Simple.Utils.
runProcess :: OutputHooks       -- ^ What to do with stdout & stderr
              -> Maybe FilePath -- ^ Working directory
              -> FilePath       -- ^ Filename of the executable
              -> [String]       -- ^ Arguments
              -> IO ExitCode    -- ^ Process exit code
runProcess outHooks cwd path args = do

  Exception.bracket
     (runInteractiveProcess path args cwd Nothing)
     (\(inh,outh,errh,_) -> hClose inh >> hClose outh >> hClose errh)
    $ \(inh,outh,errh,pid) -> do

      -- Errors are always assumed to be text (in the current locale)
      hSetBinaryMode errh False

      -- fork off a couple threads to pull on the stderr and stdout
      -- so if the process writes to stderr we do not block.

      hClose inh
      err <- hGetContents errh
      out <- hGetContents outh

      mvErr <- newEmptyMVar
      mvOut <- newEmptyMVar

      let force outputHook str = forM_ (lines str) (\l -> outputHook l)
      _ <- forkIO $ force (putStrLnHook outHooks) out >> putMVar mvOut ()
      _ <- forkIO $ force (putStrLnErrHook outHooks) err >> putMVar mvErr ()

      -- wait for both to finish, in either order
      _ <- takeMVar mvOut
      _ <- takeMVar mvErr

      -- wait for the program to terminate
      exitcode <- waitForProcess pid
      return exitcode


-- Parsing.

parseModuleName :: ReadP r String
parseModuleName = munch1 (\c -> isAlphaNum c || c == '.' || c == '/')

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
getModuleDeps :: [String] -> IO [(String, [String])]
getModuleDeps ghcArgs =
  withSystemTempDirectory "ghc-parmake" $ \tmpDir -> do
    let tmpFile = tmpDir </> "depends.mk"
    let ghcArgs' = "-M":"-dep-makefile":tmpFile:ghcArgs
    exitCode <- runProcess defaultOutputHooks Nothing "ghc" ghcArgs'
    if exitCode == ExitSuccess
      then (group . catMaybes . map parseLine . trimLines . lines) <$>
           (openFile tmpFile ReadMode >>= hGetContents)
      else return []
  where
    -- [(A.o,A.hs),(A.o,B.hi),(B.o,B.hs)] =>
    -- [(A.o,[A.hs,B.hi]), (B.o,[B.hs])]
    group :: [(String, String)] -> [(String, [String])]
    group = map (\l -> (fst . head $ l, map snd l)) .
            groupBy (\a b -> fst a == fst b)

-- Parallel 'make' engine.

-- One-way controller/worker -> logger communication.
data LogTask = LogStr String | LogStrErr String
type LogChan = Chan LogTask

-- Callbacks threaded through code that needs to output messages in a
-- thread-safe manner.
data OutputHooks = OutputHooks {
  putStrLnHook    :: !(String -> IO ()),
  putStrLnErrHook :: !(String -> IO ())
  }

defaultOutputHooks :: OutputHooks
defaultOutputHooks = OutputHooks {
  putStrLnHook = putStrLn,
  putStrLnErrHook = hPutStrLn stderr
  }

logThreadOutputHooks :: String -> LogChan -> OutputHooks
logThreadOutputHooks prefix logChan = OutputHooks {
  putStrLnHook    = \msg -> writeChan logChan $ LogStr (prefix ++ msg),
  putStrLnErrHook = \msg -> writeChan logChan $ LogStrErr (prefix ++ msg)
  }

logThread :: LogChan -> IO ()
logThread lch = forever $ do
  task <- readChan lch
  case task of
    LogStr s    -> putStrLnHook defaultOutputHooks s
    LogStrErr s -> putStrLnErrHook defaultOutputHooks s

-- One-way controller -> worker communication.
data WorkerTask = BuildModule String
type WorkerChan = Chan WorkerTask

workerThread :: [String] -> OutputHooks -> WorkerChan -> ControlChan -> IO ()
workerThread = undefined

-- One-way worker -> controller communication.
data ControlMessage = ModuleBuilt String
type ControlChan = Chan ControlMessage

controlThread :: OutputHooks -> ControlChan -> WorkerChan -> IO ()
controlThread = undefined

-- Argument handling.

getNumJobs :: [String] -> Int
getNumJobs []         = 1
getNumJobs ("-j":n:_) =
  case maybeRead n of
    Just n' -> abs n'
    Nothing -> error "The argument to '-j' must be an integer!"
getNumJobs (_:xs)     = getNumJobs xs

getGhcArgs :: [String] -> [String]
getGhcArgs []          = []
getGhcArgs ("-j":_:xs) = xs
getGhcArgs (x:xs)      = x:(getGhcArgs xs)

-- Program entry point.

main :: IO ()
main = do args <- getArgs
          let numJobs = getNumJobs args
          let ghcArgs = getGhcArgs args
          putStrLn $ "Num jobs: " ++ show numJobs
          putStrLn $ "GHC args: " ++ show ghcArgs
          putStrLn "Trying to parse module dependency information..."
          g <- getModuleDeps ghcArgs
          print g
