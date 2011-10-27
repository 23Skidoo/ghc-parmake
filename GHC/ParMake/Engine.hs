-- Parallel 'make' engine.

module GHC.ParMake.Engine
       where

import Control.Exception as E (catch, throw)
import Control.Concurrent (readChan, writeChan, Chan)
import Control.Monad (foldM, forever, unless)
import Data.Maybe (mapMaybe)
import System.Exit (ExitCode(..))
import System.FilePath (dropExtension)

import GHC.ParMake.BuildPlan (BuildPlan, Target)
import qualified GHC.ParMake.BuildPlan as BuildPlan
import GHC.ParMake.Util (defaultOutputHooks, OutputHooks(..)
                         , runProcess, upToDateCheck
                         , Verbosity, debug', notice', noticeRaw)

-- One-way controller/worker -> logger communication.
data LogTask = LogStr String | LogStrErr String
type LogChan = Chan LogTask

logThreadOutputHooks :: String -> LogChan -> OutputHooks
logThreadOutputHooks prefix logChan = OutputHooks {
  -- TODO: Fix these
  putStrHook      = undefined,
  putStrLnHook    = \msg -> writeChan logChan $ LogStr (prefix ++ msg),
  putStrErrHook   = undefined,
  putStrLnErrHook = \msg -> writeChan logChan $ LogStrErr (prefix ++ msg),
  flushStdOutHook = undefined
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

-- | Given a BuildPlan, perform the compilation.
compile :: Verbosity -> BuildPlan -> Int -> [String] -> String -> IO ExitCode
compile v p _numJobs ghcArgs outputFilename = E.catch (go 1 p) handler
  where
    handler :: ExitCode -> IO ExitCode
    handler e = return e

    totNum :: String
    totNum = show $ BuildPlan.size p

    runGHC :: [String] -> IO ()
    runGHC args =
      do debug' v $ show ("ghc":args)
         exitCode <- runProcess defaultOutputHooks Nothing "ghc" args
         unless (exitCode == ExitSuccess) (throw exitCode)

    slashesToDots = map (\s -> if s == '/' then '.' else s)

    doCompile :: (Int, BuildPlan) -> Target -> IO (Int, BuildPlan)
    doCompile (curNum, plan) target =
      do let tId   = BuildPlan.targetId target
         let tSrc  = BuildPlan.source target
         let tDeps = BuildPlan.depends target
         let plan' = BuildPlan.markCompleted plan target
         let tName = slashesToDots . dropExtension $ tSrc
         let msg = "[" ++ show curNum ++ " of "++ totNum ++ "] Compiling "
                   ++ tName
                   ++ replicate (16 - length tName) ' '
                   ++ " ( " ++ tSrc ++ ", " ++ tId ++ " )\n"
         isUpToDate <- upToDateCheck tId tDeps
         unless isUpToDate $ do noticeRaw defaultOutputHooks v msg
                                runGHC ("-c":tSrc:ghcArgs)
         return (curNum + 1, plan')

    doLink :: BuildPlan -> IO ()
    doLink plan =
      do let objs = mapMaybe BuildPlan.object $ BuildPlan.completed plan
         let cmdLine = ("-o":outputFilename:(objs ++ ghcArgs))
         isUpToDate <- upToDateCheck outputFilename objs
         unless isUpToDate (notice' v ("Linking " ++ outputFilename ++ "...")
                            >> runGHC cmdLine)

    go :: Int -> BuildPlan -> IO ExitCode
    go curNum plan =
      let rdy = BuildPlan.ready plan
      in case rdy of
         [] -> do doLink plan
                  return ExitSuccess
         _  -> do let plan' = BuildPlan.markReadyAsBuilding plan
                  (curNum', plan'') <- foldM doCompile (curNum, plan') rdy
                  go curNum' plan''
