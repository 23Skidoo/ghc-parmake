{-# LANGUAGE ScopedTypeVariables #-}

-- Parallel 'make' engine.

module GHC.ParMake.Engine
       where

import Prelude hiding (catch)
import Control.Concurrent (forkIO, newChan, readChan, writeChan, Chan)
import Control.Exception (SomeException, assert, bracket, catch)
import Control.Monad (foldM, forever, forM_, when)
import System.Exit (ExitCode(..))
import System.FilePath (dropExtension)
import System.IO (BufferMode (LineBuffering), Handle,
                  hClose, hGetLine, hPutStrLn,
                  hSetBinaryMode, hSetBuffering)
import System.Process (runInteractiveProcess)

import GHC.ParMake.BuildPlan (BuildPlan, Target)
import qualified GHC.ParMake.BuildPlan as BuildPlan
import GHC.ParMake.Util (defaultOutputHooks, OutputHooks(..)
                         , runProcess, upToDateCheck, UpToDateStatus(..)
                         , Verbosity, debug, noticeRaw, warn)

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

workerThread :: OutputHooks -> Verbosity -> String
                -> FilePath -> [String] -> [FilePath]
                -> WorkerChan -> ControlChan
                -> IO ()
workerThread outHooks verbosity totNum ghcPath ghcArgs files wch cch = do
  topHandler . withGHCServer ghcArgs $ \inh outh errh ->
    forever $ do
      task <- readChan wch
      case task of
        BuildModule curNum target ->
          do exitCode <- buildModule inh outh errh curNum target
             onSuccess exitCode (ModuleCompiled target)
               (CompileFailed target exitCode)

        BuildProgram outputFilename _objects ->
          do exitCode <- buildProgram outputFilename
             onSuccess exitCode (BuildCompleted) (BuildFailed exitCode)
  where

    topHandler :: IO () -> IO ()
    topHandler act =
      act `catch`
      (\(e :: SomeException) -> do
          warn outHooks verbosity ("Exception in 'workerThread.topHandler': "
                                   ++ show e)
          writeChan cch (BuildFailed (ExitFailure 1)))

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

    buildModule :: Handle -> Handle -> Handle -> Int -> Target -> IO ExitCode
    buildModule inh outh errh curNum target =
      do let tId   = BuildPlan.targetId target
         let tSrc  = BuildPlan.source target
         let tDeps = BuildPlan.allDepends target
         let tName = slashesToDots . dropExtension $ tSrc
         let msg reason = "[" ++ show curNum ++ " of "++ totNum ++ "] Compiling "
                          ++ tName
                          ++ replicate (16 - length tName) ' '
                          ++ " ( " ++ tSrc ++ ", " ++ tId ++ " ) [" ++ reason ++ "]\n"
             compileBecause reason = do noticeRaw outHooks verbosity (msg reason)
                                        compileWithGhcServer curNum tSrc inh outh errh
         upToDateStatus <- upToDateCheck tId tDeps
         case upToDateStatus of
           UpToDate             -> return ExitSuccess
           TargetDoesNotExist   -> compileBecause "new"
           NewerDependency file -> compileBecause (file ++ " changed")

    -- See the 'protocol for communication' note in GhcServer.hs.
    compileWithGhcServer :: Int -> FilePath -> Handle -> Handle -> Handle
                            -> IO ExitCode
    compileWithGhcServer jobNum tSrc inh outh errh =
      submitJob >> redirectGHCOutput >> parseResult
      where
        submitJob :: IO ()
        submitJob = do
          debug outHooks verbosity $ "Submitting job number " ++ (show jobNum)
          hPutStrLn inh ("JOB " ++ show jobNum)
          hPutStrLn inh ("COMPILE " ++ tSrc)
          hPutStrLn inh "END"

        redirectGHCOutput :: IO ()
        redirectGHCOutput = do
          debug outHooks verbosity $
            "Reading compiler output for job number " ++ (show jobNum)
          header <- hGetLine errh
          case header of
            'O':'U':'T':'P':'U':'T':' ':path -> assert (path == tSrc) (return ())
            other -> unexpected other

          output <- go []
          mapM_ (putStrLnErrHook outHooks) output

            where
              err e = error $ "GHC.ParMake.Engine.redirectGHCOutput: " ++ e
              unexpected inp = err $ "unexpected input: " ++ show inp

              go acc = do
                l <- hGetLine errh
                case l of
                  "END" -> return (reverse acc)
                  _     -> go (l:acc)

        parseResult :: IO ExitCode
        parseResult = do
          debug outHooks verbosity $
            "Parsing result for the job number " ++ (show jobNum)
          header <- hGetLine outh
          let jobNum' = case header of
                'J':'O':'B':'_':'D':'O':'N':'E':' ':rest -> read rest
                other -> unexpected other
          when (jobNum /= jobNum') $
            err "job numbers mismatch"

          resStr <- hGetLine outh
          let res = case resStr of
                'S':'U':'C':'C':'E':'S':'S':' ':path ->
                  assert (path == tSrc) ExitSuccess
                'F':'A':'I':'L':'U':'R':'E':' ':path ->
                  assert (path == tSrc) (ExitFailure 1)
                other -> unexpected other

          end <- hGetLine outh
          case end of
            "END" -> return res
            other -> unexpected other

            where
              err e = error $ "GHC.ParMake.Engine.parseResult: " ++ e
              unexpected inp = err $ "unexpected input: " ++ show inp


    buildProgram :: FilePath -> IO ExitCode
    buildProgram outputFilename =
      runGHC ("--make":"-o":outputFilename:(files ++ ghcArgs))

    runGHC :: [String] -> IO ExitCode
    runGHC args =
      do debug outHooks verbosity $ show (ghcPath:args)
         runProcess outHooks Nothing ghcPath args

    withGHCServer :: [String] ->
                     (Handle -> Handle -> Handle -> IO ()) -> IO ()
    withGHCServer args action = do
      debug outHooks verbosity $ show (ghcServerPath:args)
      bracket
        (runInteractiveProcess ghcServerPath args Nothing Nothing)
        (\(inh, outh, errh, _) -> hClose inh >> hClose outh >> hClose errh)
        (\(inh, outh, errh, _) -> do hSetBinaryMode inh  False
                                     hSetBinaryMode outh False
                                     hSetBinaryMode errh False
                                     hSetBuffering inh  LineBuffering
                                     hSetBuffering outh LineBuffering
                                     hSetBuffering errh LineBuffering
                                     action inh outh errh)

    ghcServerPath :: FilePath
    ghcServerPath = "ghc-server"


-- One-way worker -> controller communication.
data ControlMessage = ModuleCompiled Target | BuildCompleted
                    | CompileFailed Target ExitCode | BuildFailed ExitCode
                    deriving (Show)
type ControlChan = Chan ControlMessage

controlThread :: BuildPlan -> Maybe FilePath -> ControlChan -> WorkerChan
                 -> IO ExitCode
controlThread p mOutputFilename cch wch =
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

                -- Check if there is more to do.
                if (null rdy && BuildPlan.numBuilding plan'' == 0)

                  -- All modules are done.
                  then
                    -- Do we want to build an executable?
                    -- If yes, queue this as the last thing to do before
                    -- shutting down.
                    case mOutputFilename of
                      Nothing             -> return ExitSuccess
                      Just outputFilename -> do
                        writeChan wch $ BuildProgram outputFilename
                                                     (BuildPlan.objects plan'')
                        -- Wait for the response to BuildProgram.
                        -- It must only be build success or failure.
                        buildProgramMsg <- readChan cch
                        case buildProgramMsg of
                          BuildFailed c  -> return c
                          BuildCompleted -> return ExitSuccess
                          x              ->
                            error $ "GHC.ParMake.Engine.controlThread: "
                            ++ "Unexpected BuildProgram response: " ++ show x

                  else go plan'' curNum'

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
compile :: Verbosity -> BuildPlan -> Int
           -> FilePath -> [String] -> [FilePath] -> Maybe FilePath
           -> IO ExitCode
compile verbosity plan numJobs ghcPath ghcArgs files mOutputFilename =
  do
    -- Init comm. channels
    workerChan  <- newChan
    logChan     <- newChan
    controlChan <- newChan

    -- Fork off worker threads.
    forM_ [1..numJobs]
      (\n -> forkIO $ workerThread
             (logThreadOutputHooks
              (if numJobs == 1 then "" else "[" ++ show n ++ "]") logChan)
             verbosity totNum ghcPath ghcArgs files workerChan controlChan)

    -- Fork off log thread.
    _ <- ($) forkIO $ logThread logChan

    -- Start the control thread.
    controlThread plan mOutputFilename controlChan workerChan

    -- Note that we don't explicitly shut down the worker threads;
    -- the runtime kills them when the main thread exits.
  where
    totNum :: String
    totNum = show $ BuildPlan.size plan
