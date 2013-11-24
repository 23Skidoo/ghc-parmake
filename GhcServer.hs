{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
       where

#if !MIN_VERSION_base(4,6,0)
import Prelude hiding (catch)
#endif

import qualified GHC
import qualified GHC.Paths
import qualified DriverPipeline
import qualified DriverPhases

-- The liftIO here is not the same one in Control.Monad.Trans!
-- GHC defines its own MonadIO.
import MonadUtils (MonadIO, liftIO)

import Control.Monad      (forever, void, when)
import Control.Exception  (SomeException, catch)
import System.Environment (getArgs)
import System.IO          (BufferMode (LineBuffering), Handle,
                           hGetLine, hPutStrLn,
                           hSetBuffering, hSetBinaryMode,
                           stderr, stdin, stdout)

-- Protocol for communication between ghc-parmake and ghc-server:

-- ghc-parmake -> ghc-server (stdin):

-- JOB JOB_NUMBER
-- COMPILE PATH 1
-- COMPILE PATH 2
-- COMPILE PATH 3
-- ...
-- END

-- ghc-server -> ghc-parmake (stdout)
-- JOB_DONE JOB_NUMBER
-- SUCCESS PATH 1
-- SUCCESS PATH 2
-- FAILURE PATH 3
-- ...
-- END

-- ghc-server -> ghc-parmake (stderr)
-- OUTPUT PATH_1
-- Build output line 1
-- Build output line 2
-- ...
-- END
-- OUTPUT PATH_2
-- ...

data Job = Job !Int ![FilePath]
           deriving Show
data Result = Success !FilePath | Failure !FilePath
            deriving Show

main :: IO ()
main = do
  hSetBinaryMode stdin  False
  hSetBinaryMode stdout False
  hSetBinaryMode stderr False

  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  inGhcMonad $
    forever $ do
      job     <- liftIO $ getJob stdin
      results <- executeJob job stderr
      liftIO $ printResults job results stdout

inGhcMonad :: GHC.Ghc a -> IO a
inGhcMonad ghcMonadAction = do
  argv0 <- getArgs
  let argv1 = map (GHC.mkGeneralLocated "on the commandline") argv0
  (argv2, staticFlagWarnings) <- GHC.parseStaticFlags argv1
  GHC.runGhc (Just GHC.Paths.libdir) $ do
    origDFlags <- GHC.getSessionDynFlags
    (dflags, leftover, dynFlagWarnings) <-
      GHC.parseDynamicFlags origDFlags argv2
    let warns = map GHC.unLoc $ staticFlagWarnings ++ dynFlagWarnings
    when (not . null $ warns) $
      error $ "warnings parsing flags: " ++ show warns
    when (not . null $ leftover) $
      error $ "unparsed args: " ++ show (map GHC.unLoc leftover)
    void . GHC.setSessionDynFlags $
      dflags { GHC.ghcLink = GHC.NoLink
             , GHC.ghcMode = GHC.OneShot }
    ghcMonadAction

getJob :: Handle -> IO Job
getJob handle = do
  jobId   <- parseHeader
  jobMods <- parseModulePaths
  return (Job jobId jobMods)
  where
    parseHeader :: IO Int
    parseHeader = do
      l <- hGetLine handle
      case l of
        'J':'O':'B':' ':rest -> return (read rest)
        _                    -> error "GhcServer.parseHeader: unexpected input"

    parseModulePaths :: IO [FilePath]
    parseModulePaths = do
      mods <- go []
      return mods
      where
        go :: [FilePath] -> IO [FilePath]
        go acc = do
          l <- hGetLine handle
          case l of
            'C':'O':'M':'P':'I':'L':'E':' ':path -> go (path:acc)
            "END" -> return (reverse acc)
            _     -> error "GhcServer.parseModulePaths: parse error"

executeJob :: (GHC.GhcMonad m) => Job -> Handle -> m [Result]
executeJob (Job _jobId jobModules) handle = do
  sequence [ compileModule path | path <- jobModules]
  where
    compileModule :: (GHC.GhcMonad m) => FilePath -> m Result
    compileModule path = do
      liftIO $ hPutStrLn handle ("OUTPUT " ++ path)
      ret <- doCompileModule path
      liftIO $ hPutStrLn handle "END"
      return $ if ret then (Success path) else (Failure path)

    doCompileModule :: (GHC.GhcMonad m) => FilePath -> m Bool
    doCompileModule path = do
      hsc_env <- GHC.getSession
      liftIO $ (compile hsc_env) `catch` \(exc :: SomeException) -> do
        hPutStrLn handle (show exc)
        return False
        where
          compile hsc_env = do
            void $ DriverPipeline.compileFile hsc_env DriverPhases.StopLn
              (path, Nothing)
            return True

printResults :: Job -> [Result] -> Handle -> IO ()
printResults (Job jobId _) results handle = do
  hPutStrLn handle $ "JOB_DONE " ++ (show jobId)
  mapM_ printResult results
  hPutStrLn handle "END"
    where
      printResult :: Result -> IO ()
      printResult (Success fp) = hPutStrLn handle ("SUCCESS " ++ fp)
      printResult (Failure fp) = hPutStrLn handle ("FAILURE " ++ fp)
