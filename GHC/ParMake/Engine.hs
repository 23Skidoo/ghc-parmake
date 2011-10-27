-- Parallel 'make' engine.

module GHC.ParMake.Engine
       where

import Control.Exception as E (catch, throw)
import Control.Concurrent (readChan, writeChan, Chan)
import Control.Monad (filterM, foldM, forever, liftM, unless)
import Data.List ((\\))
import Data.Maybe (mapMaybe)
import System.Directory (doesFileExist, getModificationTime)
import System.Exit (ExitCode(..))
import System.FilePath (dropExtension)

import GHC.ParMake.BuildPlan (BuildPlan, Target)
import qualified GHC.ParMake.BuildPlan as BuildPlan
import GHC.ParMake.Common (andM)
import GHC.ParMake.Util (defaultOutputHooks, OutputHooks(..), runProcess,
                         Verbosity, debug', notice', noticeRaw)

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

-- | Is this target up to date w.r.t. its dependencies?
upToDateCheck :: FilePath -> [FilePath] -> IO Bool
upToDateCheck tId tDeps =
  do tExists <- doesFileExist tId
     if not tExists
       then return False
       else do tModTime <- getModificationTime tId
               -- TOFIX: Is this check correct? How GHC does this?
               andM [ liftM (tModTime >=) (getModificationTime depId)
                    | depId <- tDeps]

-- | Given a BuildPlan, perform the compilation.
compile :: Verbosity -> BuildPlan -> Int -> [String] -> String -> IO ExitCode
compile v plan n as ofn =
  E.catch (processInitial plan []
           >>= \plan' -> compile' v plan' n (countTotNum plan') as ofn) handler
  where
    handler :: ExitCode -> IO ExitCode
    handler e = return e

    -- Process the graph so that only non-up-to-date targets are left in 'ready'.
    -- TODO: This can be optimised a bit if moved to BuildPlan.
    processInitial :: BuildPlan -> [Target] -> IO BuildPlan
    processInitial p cache =
      do let rdy = BuildPlan.ready p
         let rdy' = filter (\t -> not $ t `elem` cache) rdy
         rdy'' <- filterM
                  (\t -> upToDateCheck (BuildPlan.targetId t)
                         (BuildPlan.depends t)) rdy'
         case rdy'' of
           [] -> return p
           _  -> do let p'  = BuildPlan.markAllBuilding p rdy''
                    let p'' = BuildPlan.markAllCompleted p' rdy''
                    processInitial p'' (rdy \\ rdy'')

    countTotNum :: BuildPlan -> Int
    countTotNum p = BuildPlan.countReachable p (BuildPlan.ready p)


-- | Actual implementation of `compile`.
compile' :: Verbosity -> BuildPlan -> Int -> Int -> [String] -> String
            -> IO ExitCode
compile' v p _numJobs totNum ghcArgs outputFilename = go 1 p
  where

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
         -- TODO: This is buggy when -odir is specified
         let tName = slashesToDots . dropExtension $ tId
         let msg = "[" ++ show curNum ++ " of (at most) "
                   ++ show totNum ++ "] Compiling "
                   ++ tName
                   ++ (let l = length tName
                       in if l < 25 then (replicate (25 - l) ' ')
                          else ('\n':replicate 54 ' '))
                   ++ " ( " ++ tSrc ++ ", " ++ tId ++ " )\n"
         isUpToDate <- upToDateCheck tId tDeps
         if isUpToDate
           then return (curNum, plan')
           else do noticeRaw defaultOutputHooks v msg
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
