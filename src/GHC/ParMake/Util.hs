-- Various utility functions for interfacing with the outside world. Most of
-- this is taken from Distribution.Simple.Utils.

module GHC.ParMake.Util (runProcess, upToDateCheck
                        , defaultOutputHooks, OutputHooks(..)
                        , warn, notice, info, debug
                        , warn', notice', noticeRaw, info', debug'
                        , Verbosity, intToVerbosity
                        , silent, normal, verbose, deafening)
       where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_, liftM, when)
import qualified Control.Exception as Exception
import System.Directory (doesFileExist, getModificationTime)
import System.Exit (ExitCode(..))
import System.IO ( hClose, hGetContents, hFlush, hPutStr, hPutStrLn
                   , hSetBinaryMode, stderr, stdout)
import System.Process (runInteractiveProcess, waitForProcess)

import GHC.ParMake.Common (andM)

-- Copied from Distribution.Verbosity.
data Verbosity = Silent | Normal | Verbose | Deafening
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- We shouldn't print /anything/ unless an error occurs in silent mode
silent :: Verbosity
silent = Silent

-- Print stuff we want to see by default
normal :: Verbosity
normal = Normal

-- Be more verbose about what's going on
verbose :: Verbosity
verbose = Verbose

-- Not only are we verbose ourselves (perhaps even noisier than when
-- being "verbose"), but we tell everything we run to be verbose too
deafening :: Verbosity
deafening = Deafening

intToVerbosity :: Int -> Maybe Verbosity
intToVerbosity 0 = Just Silent
intToVerbosity 1 = Just Normal
intToVerbosity 2 = Just Verbose
intToVerbosity 3 = Just Deafening
intToVerbosity _ = Nothing

-- | Non fatal conditions that may be indicative of an error or problem.
--
-- We display these at the 'normal' verbosity level.
--
warn :: OutputHooks -> Verbosity -> String -> IO ()
warn outHooks verbosity msg =
  when (verbosity >= normal) $
    putStrErrHook outHooks (wrapText ("Warning: " ++ msg))

warn' :: Verbosity -> String -> IO ()
warn' = warn defaultOutputHooks

-- | Useful status messages.
--
-- We display these at the 'normal' verbosity level.
--
-- This is for the ordinary helpful status messages that users see. Just
-- enough information to know that things are working but not floods of detail.
--
noticeRaw :: OutputHooks -> Verbosity -> String -> IO ()
noticeRaw outHooks verbosity msg =
  when (verbosity >= normal) $ do
    flushStdOutHook outHooks
    putStrHook outHooks msg

notice :: OutputHooks -> Verbosity -> String -> IO ()
notice h v msg = noticeRaw h v (wrapText msg)

notice' :: Verbosity -> String -> IO ()
notice' = notice defaultOutputHooks

-- | More detail on the operation of some action.
--
-- We display these messages when the verbosity level is 'verbose'
--
info :: OutputHooks -> Verbosity -> String -> IO ()
info outHooks verbosity msg =
  when (verbosity >= verbose) $
    putStrHook outHooks (wrapText msg)

info' :: Verbosity -> String -> IO ()
info' = info defaultOutputHooks

-- | Detailed internal debugging information
--
-- We display these messages when the verbosity level is 'deafening'
--
debug :: OutputHooks -> Verbosity -> String -> IO ()
debug outHooks verbosity msg =
  when (verbosity >= deafening) $ do
    putStrHook outHooks (wrapText msg)
    flushStdOutHook outHooks

debug' :: Verbosity -> String -> IO ()
debug' = debug defaultOutputHooks

-- Callbacks threaded through code that needs to output messages in a
-- thread-safe manner.
data OutputHooks = OutputHooks {
  putStrHook      :: !(String -> IO ()),
  putStrLnHook    :: !(String -> IO ()),
  putStrErrHook   :: !(String -> IO ()),
  putStrLnErrHook :: !(String -> IO ()),
  flushStdOutHook :: !(IO ())
  }

defaultOutputHooks :: OutputHooks
defaultOutputHooks = OutputHooks {
  putStrHook      = putStr,
  putStrLnHook    = putStrLn,
  putStrErrHook   = hPutStr stderr,
  putStrLnErrHook = hPutStrLn stderr,
  flushStdOutHook = hFlush stdout
  }

-- | Process creation.
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
      err <- hGetContents errh -- lazy IO!
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


-- | Wraps text to the default line width. Existing newlines are preserved.
wrapText :: String -> String
wrapText = unlines
         . concatMap (map unwords
                    . wrapLine 79
                    . words)
         . lines

-- | Wraps a list of words to a list of lines of words of a particular width.
wrapLine :: Int -> [String] -> [[String]]
wrapLine width = wrap 0 []
  where wrap :: Int -> [String] -> [String] -> [[String]]
        wrap 0   []   (w:ws)
          | length w + 1 > width
          = wrap (length w) [w] ws
        wrap col line (w:ws)
          | col + length w + 1 > width
          = reverse line : wrap 0 [] (w:ws)
        wrap col line (w:ws)
          = let col' = col + length w + 1
             in wrap col' (w:line) ws
        wrap _ []   [] = []
        wrap _ line [] = [reverse line]

-- | Is this target up to date w.r.t. its dependencies?
upToDateCheck :: FilePath -> [FilePath] -> IO Bool
upToDateCheck tId tDeps =
  do tExists <- doesFileExist tId
     if not tExists
       then return False
       else do tModTime <- getModificationTime tId
               -- TODO: Is this check correct? How GHC does this?
               andM [ liftM (tModTime >=) (getModificationTime depId)
                    | depId <- tDeps]
