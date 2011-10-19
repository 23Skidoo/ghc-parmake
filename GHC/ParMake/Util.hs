-- Various utility functions for interfacing with the outside world.

module GHC.ParMake.Util (runProcess, defaultOutputHooks, OutputHooks(..))
       where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_)
import qualified Control.Exception as Exception
import System.Exit (ExitCode(..))
import System.IO (hClose, hGetContents, hPutStrLn, hSetBinaryMode, stderr)
import System.Process (runInteractiveProcess, waitForProcess)

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
