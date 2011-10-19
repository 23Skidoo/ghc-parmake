-- Parallel 'make' engine.

module GHC.ParMake.Engine
       where

import Control.Concurrent (readChan, writeChan, Chan)
import Control.Monad (forever)

import GHC.ParMake.BuildPlan (BuildPlan)
import GHC.ParMake.Util (defaultOutputHooks, OutputHooks(..))

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

compileInParallel :: BuildPlan -> IO ()
compileInParallel _ = return ()
