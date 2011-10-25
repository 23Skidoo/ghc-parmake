-- Parallel 'make' engine.

module GHC.ParMake.Engine
       where

import Control.Exception as E (catch, throw)
import Control.Concurrent (readChan, writeChan, Chan)
import Control.Monad (foldM, forever, unless)
import System.Directory (doesFileExist, getModificationTime)
import System.Exit (ExitCode(..))
import System.FilePath (replaceExtension)

import GHC.ParMake.BuildPlan (BuildPlan, Target)
import qualified GHC.ParMake.BuildPlan as BuildPlan
import GHC.ParMake.Util (defaultOutputHooks, OutputHooks(..), runProcess)

-- One-way controller/worker -> logger communication.
data LogTask = LogStr String | LogStrErr String
type LogChan = Chan LogTask

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

compile :: BuildPlan -> Int -> [String] -> IO ExitCode
compile p _ ghcArgs = E.catch (go p) handler
  where
    handler :: ExitCode -> IO ExitCode
    handler e = return e

    runGHC :: [String] -> IO ()
    runGHC args =
      do print $ "ghc":args
         exitCode <- runProcess defaultOutputHooks Nothing "ghc" args
         unless (exitCode == ExitSuccess) (throw exitCode)

    upToDateCheck :: FilePath -> [FilePath] -> IO Bool
    upToDateCheck tId tDeps =
      do tExists <- doesFileExist tId
         if not tExists
           then return False
           else checkModificationTime tId tDeps

    checkModificationTime :: FilePath -> [FilePath] -> IO Bool
    checkModificationTime tId tDeps =
      do tModTime <- getModificationTime tId
         foldM (\c depId -> if not c then return c
                            else do depModTime <- getModificationTime depId
                                    return $ depModTime < tModTime
               ) True tDeps

    doCompile :: BuildPlan -> Target -> IO BuildPlan
    doCompile plan target =
      do let tId   = BuildPlan.targetId target
         let tSrcId = replaceExtension tId ".hs" -- HACK
         let tDeps = BuildPlan.depends target
         let plan' = BuildPlan.markCompleted plan target
         isUpToDate <- upToDateCheck tId tDeps
         unless isUpToDate (putStrLn tId
                            >> runGHC ("-c":tSrcId:ghcArgs))
         return plan'

    doLink :: BuildPlan -> IO ()
    doLink plan =
      do let objs = map BuildPlan.targetId $ BuildPlan.completed plan
         putStrLn "Linking..."
         runGHC (objs ++ ghcArgs)

    go :: BuildPlan -> IO ExitCode
    go plan =
      let rdy = BuildPlan.ready plan
      in if null rdy
         then
           do doLink plan
              putStrLn "DONE"
              return ExitSuccess
         else
           do let plan' = BuildPlan.markReadyAsBuilding plan
              plan'' <- foldM doCompile plan' rdy
              go plan''
