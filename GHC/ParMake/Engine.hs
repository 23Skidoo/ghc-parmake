-- Parallel 'make' engine.

module GHC.ParMake.Engine
       where

import Control.Concurrent (forkIO, newChan, readChan, writeChan, Chan)
import Control.Monad (foldM, forever, forM_, when)
import System.Exit (ExitCode(..))
import System.FilePath (dropExtension)

import GHC.ParMake.BuildPlan (BuildPlan, Target)
import qualified GHC.ParMake.BuildPlan as BuildPlan
import GHC.ParMake.Util (defaultOutputHooks, OutputHooks(..)
                         , runProcess, upToDateCheck
                         , Verbosity, debug, notice, noticeRaw)

-- The program consists of several threads which communicate via Chans. There
-- are several worker threads, which compile the modules. A single control
-- thread maintains the module graph and assigns tasks to the worker threads. A
-- single logger thread prints out messages received from the worker threads.

-- After the worker thread compiles a module, it notifies the controller thread,
-- which then updates the module graph and adds new tasks for the worker threads
-- (if possible). The control thread terminates when the last module has been
-- built (which leads to the termination of all other threads).

-- One-way controller/worker -> logger communication.
data LogTask = LogStr String | LogStrLn String
             | LogStrErr String | LogStrLnErr String
             | LogFlushStdOut
type LogChan = Chan LogTask

logThreadOutputHooks :: String -> LogChan -> OutputHooks
logThreadOutputHooks prefix logChan = OutputHooks {
  putStrHook      = \msg -> writeChan logChan $ LogStr (prefix ++ msg),
  putStrLnHook    = \msg -> writeChan logChan $ LogStrLn (prefix ++ msg),
  putStrErrHook   = \msg -> writeChan logChan $ LogStrErr msg,
  putStrLnErrHook = \msg -> writeChan logChan $ LogStrLnErr msg,
  flushStdOutHook = writeChan logChan LogFlushStdOut
  }

logThread :: LogChan -> IO ()
logThread lch = forever $ do
  task <- readChan lch
  case task of
    LogStr s       -> putStrHook defaultOutputHooks s
    LogStrLn s     -> putStrLnHook defaultOutputHooks s
    LogStrErr s    -> putStrErrHook defaultOutputHooks s
    LogStrLnErr s  -> putStrLnErrHook defaultOutputHooks s
    LogFlushStdOut -> flushStdOutHook defaultOutputHooks

-- One-way controller -> worker communication.
data WorkerTask = BuildModule Int Target | BuildProgram FilePath [FilePath]
type WorkerChan = Chan WorkerTask

workerThread :: OutputHooks -> Verbosity -> String -> [String]
                -> WorkerChan -> ControlChan
                -> IO ()
workerThread outHooks verbosity totNum ghcArgs wch cch
  = forever $ do
    task <- readChan wch
    case task of
      BuildModule curNum target ->
        do exitCode <- buildModule curNum target
           onSuccess exitCode (ModuleCompiled target)
             (CompileFailed target exitCode)

      BuildProgram outputFilename objects ->
        do exitCode <- buildProgram outputFilename objects
           onSuccess exitCode (BuildCompleted) (BuildFailed exitCode)
  where

    runGHC :: [String] -> IO ExitCode
    runGHC args =
      do debug outHooks verbosity $ show ("ghc":args)
         runProcess outHooks Nothing "ghc" args

    onSuccess :: ExitCode -> ControlMessage -> ControlMessage -> IO ()
    onSuccess exitCode msgSucc msgFail =
      if exitCode == ExitSuccess
      then writeChan cch msgSucc
      else writeChan cch msgFail

    slashesToDots :: String -> String
    slashesToDots = map slashToDot
      where
        slashToDot '/' = '.'
        slashToDot c   = c

    buildModule :: Int -> Target -> IO ExitCode
    buildModule curNum target =
      do let tId   = BuildPlan.targetId target
         let tSrc  = BuildPlan.source target
         let tDeps = BuildPlan.depends target
         let tName = slashesToDots . dropExtension $ tSrc
         let msg = "[" ++ show curNum ++ " of "++ totNum ++ "] Compiling "
                   ++ tName
                   ++ replicate (16 - length tName) ' '
                   ++ " ( " ++ tSrc ++ ", " ++ tId ++ " )\n"
         isUpToDate <- upToDateCheck tId tDeps
         if not isUpToDate
           then
           do noticeRaw outHooks verbosity msg
              runGHC ("-c":tSrc:ghcArgs)
           else
           return ExitSuccess

    buildProgram :: FilePath -> [FilePath] -> IO ExitCode
    buildProgram outputFilename objs  =
      do let cmdLine = ("-o":outputFilename:(objs ++ ghcArgs))
         isUpToDate <- upToDateCheck outputFilename objs
         if not isUpToDate
           then do notice outHooks verbosity
                     ("Linking " ++ outputFilename ++ "...")
                   runGHC cmdLine
           else return ExitSuccess


-- One-way worker -> controller communication.
data ControlMessage = ModuleCompiled Target | BuildCompleted
                    | CompileFailed Target ExitCode | BuildFailed ExitCode
type ControlChan = Chan ControlMessage

controlThread :: BuildPlan -> FilePath -> ControlChan -> WorkerChan
                 -> IO ExitCode
controlThread p outputFilename cch wch =
  do let rdy = BuildPlan.ready p
     -- Give worker threads initial tasks.
     curNum <- postTasks rdy 1

     -- Shouldn't happen
     if null rdy
       then return ExitSuccess
       else go (BuildPlan.markReadyAsBuilding p) curNum
  where
    -- Stuff a bunch of tasks into the controller -> workers comm. channel.
    postTasks :: [Target] -> Int -> IO Int
    postTasks rdy curNum =
      foldM (\curNum' t -> do writeChan wch (BuildModule curNum' t)
                              return $ curNum' + 1) curNum rdy

    -- Main loop.
    go :: BuildPlan -> Int -> IO ExitCode
    go plan curNum =
      do msg <- readChan cch
         case msg of
           ModuleCompiled target ->
             do let plan' = BuildPlan.markCompleted plan target
                let rdy   = BuildPlan.ready plan'
                curNum'  <- postTasks rdy curNum
                let plan'' = BuildPlan.markReadyAsBuilding plan'
                when (null rdy && BuildPlan.numBuilding plan'' == 0)
                  (writeChan wch (BuildProgram outputFilename
                                  $ BuildPlan.objects plan''))
                go plan'' curNum'

           CompileFailed t c -> waitAndExit (BuildPlan.markCompleted plan t) c

           BuildCompleted  -> return ExitSuccess
           BuildFailed c   -> return c


    -- One of the worker threads encountered an error. Wait for all threads to
    -- finish.
    waitAndExit :: BuildPlan -> ExitCode -> IO ExitCode
    waitAndExit plan exitCode =
      if BuildPlan.numBuilding plan > 0
      then
        do msg <- readChan cch
           case msg of
             ModuleCompiled target ->
               waitAndExit (BuildPlan.markCompleted plan target) exitCode

             CompileFailed target _ ->
               waitAndExit (BuildPlan.markCompleted plan target) exitCode

             -- Can't happen.
             BuildCompleted -> return exitCode
             BuildFailed _  -> return exitCode

      else return exitCode

-- | Given a BuildPlan, perform the compilation.
compile :: Verbosity -> BuildPlan -> Int -> [String] -> String -> IO ExitCode
compile verbosity plan numJobs ghcArgs outputFilename =
  do
    -- Init comm. channels
    workerChan  <- newChan
    logChan     <- newChan
    controlChan <- newChan

    -- Fork off worker threads.
    forM_ [1..numJobs]
      (\n -> forkIO $ workerThread
             (logThreadOutputHooks ("[" ++ show n ++ "]") logChan)
             verbosity totNum ghcArgs workerChan controlChan)

    -- Fork off log thread.
    _ <- ($) forkIO $ logThread logChan

    -- Start the control thread.
    controlThread plan outputFilename controlChan workerChan
  where
    totNum :: String
    totNum = show $ BuildPlan.size plan
