module Main
       where

import Control.Monad (liftM, unless, when)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure, exitSuccess, exitWith)
import System.FilePath (dropExtension)
import System.IO (hPutStrLn, stderr)

import GHC.ParMake.Common (maybeRead)
import GHC.ParMake.Util

import qualified GHC.ParMake.BuildPlan as BuildPlan
import qualified GHC.ParMake.Parse as Parse
import qualified GHC.ParMake.Engine as Engine

-- Argument handling.

data Args = Args {
  verbosity      :: Verbosity,
  printVersion   :: Bool,
  printUsage     :: Bool,
  numJobs        :: Int,
  ghcPath        :: String,
  extraDepends   :: [String],
  outputFilename :: Maybe String,
  osuf           :: String,
  hisuf          :: String,
  skipFinalPass  :: Bool
  } deriving Show

defaultArgs :: Args
defaultArgs = Args {
  verbosity      = normal,
  printVersion   = False,
  printUsage     = False,
  numJobs        = 1,
  ghcPath        = "ghc",
  extraDepends   = [],
  outputFilename = Nothing,
  osuf           = "o",
  hisuf          = "hi",
  skipFinalPass  = False
  }

parseArgs :: [String] -> Args
parseArgs l = go l defaultArgs
  where
    parseError s = error $ "ghc-parmake: Main.parseArgs: " ++ s
    parseNumJobs n = fromMaybe (parseError "The argument to '-j' must be an integer!")
                     (liftM abs $ maybeRead n)
    parseVerbosity n = fromMaybe verbose (maybeRead n >>= intToVerbosity)

    go [] acc                        = acc
    go ("-V":_) acc                  = acc { printVersion = True }
    go ("--help":_) acc              = acc { printUsage = True }
    go ("-j":n:as) acc               = go as $ acc { numJobs = parseNumJobs n }
    go (('-':'j':n:[]):as) acc       = go as $ acc { numJobs = parseNumJobs [n] }
    go (('-':'v':n:[]):as) acc       = go as $
                                       acc { verbosity = parseVerbosity [n] }
    go (('-':'v':'v':n:[]):as) acc   = go as $
                                       acc { verbosity = parseVerbosity [n] }
    go ("-v":as) acc                 = go as $ acc { verbosity = verbose }
    -- Add -optP-include -optPmyfile as extraDepends
    go ("-optP-include":optPfile:as) acc@Args{ extraDepends = ds }
                                     = case splitOffPrefix "-optP" optPfile of
                                         Just path | path /= [] ->
                                           go as $ acc { extraDepends = path : ds }
                                         Just _ -> parseError "path given after -optP-include is empty!"
                                         _      -> parseError "missing -optP after -optP-include"
    go ("-o":n:as) acc               = go as $ acc { outputFilename = Just n }
    go ("-osuf":suf:as) acc          = go as $ acc { osuf = suf }
    go ("-hisuf":suf:as) acc         = go as $ acc { hisuf = suf }
    go ("--skip-final-pass":as) acc   = go as $ acc { skipFinalPass = True }
    go ("--ghc-path":p:as) acc       = go as $ acc { ghcPath = p }
    go (a:as) acc
      | "--ghc-path=" `isPrefixOf` a = let (o,p') = break (== '=') a in
                                       go (o:(tail p'):as) acc
    go (_:as) acc                    = go as acc


splitOffPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
splitOffPrefix p s = case splitAt (length p) s of
  (p', r) | p' == p -> Just r
  _                 -> Nothing


getGhcArgs :: [String] -> ([String],[String])
getGhcArgs argv = let (as, fs) = getGhcArgs' argv [] []
                  in (reverse as, reverse fs)
  where
    pgmSuffixes = ["L", "P", "c", "m", "s", "a", "l", "dll", "F", "windres"]
    optsWithArg = [ "-odir", "-hidir", "-ohi", "-stubdir", "-outputdir"
                  , "-tmpdir", "-osuf", "-hisuf", "-hcsuf"
                  , "-package", "-package-db", "-package-id", "-hide-package"
                  , "-ignore-package", "-package-name", "-package-conf", "-f"
                  , "-framework", "-framework-path"
                  , "-main-is", "-x"]
                  ++ ["-pgm" ++ str | str <- pgmSuffixes ]
                  ++ ["-opt" ++ str | str <- pgmSuffixes ]

    eatOption []           as  = ([], as)
    eatOption (opt:arg:xs) as
      -- Unlike 'ghc --make', 'ghc -c' for some reason does not include -hidir
      -- in the interface search path.
      | opt == "-hidir"        = (xs, ('-':'i':arg):arg:opt:as)
      | opt `elem` optsWithArg = (xs, arg:opt:as)
    eatOption (x:xs) as        = (xs, x:as)

    getGhcArgs' [] as fs                      = (as, fs)
    -- Options not passed to GHC: -o, -j, -vv, --ghc-path, --make.
    getGhcArgs' ("-j":_:xs) as fs             = getGhcArgs' xs as fs
    getGhcArgs' ("-o":_:xs) as fs             = getGhcArgs' xs as fs
    getGhcArgs' (('-':'v':'v':_:[]):xs) as fs = getGhcArgs' xs as fs
    getGhcArgs' ("--skip-final-pass":xs) as fs = getGhcArgs' xs as fs
    getGhcArgs' ("--ghc-path":_:xs)     as fs = getGhcArgs' xs as fs
    getGhcArgs' (x:xs) as fs
      | "--ghc-path=" `isPrefixOf` x          = getGhcArgs' xs as fs
    getGhcArgs' ("--make":xs) as fs           = getGhcArgs' xs as fs
    getGhcArgs' xs@(('-':_):_) as fs          = let (xs', as') = eatOption xs as
                                                in getGhcArgs' xs' as' fs
    getGhcArgs' (x:xs) as fs                  = getGhcArgs' xs as (x:fs)

usage :: IO ()
usage =
  putStr $ "Usage: ghc-parmake [OPTIONS] FILES\n" ++
  "A parallel wrapper around 'ghc --make'.\n\n" ++
  "Options: \n" ++
  "-j N             - Run N jobs in parallel.\n" ++
  "--skip-final-pass - Skip the final ghc --make pass.\n" ++
  "                    Saves a few seconds, but TH changes might not be noticed.\n" ++
  "--ghc-path=PATH  - Set the path to the ghc executable.\n" ++
  "-vv[N]           - Set verbosity to N (only for ghc-parmake). " ++
  "N is 0-3, default 1.\n" ++
  "-v[N]            - Set verbosity to N " ++
  "(both for GHC and ghc-parmake itself).\n" ++
  "--help           - Print usage information.\n" ++
  "-V               - Print version information.\n" ++
  "\nOther options are passed to GHC unmodified.\n"

-- TODO: To fully emulate GHC's behaviour, we must know whether the input module
-- set contains a module Main.
--
-- Consider these two invocations:
--
-- $ ghc --make Module.hs Module0.hs
-- [1 of 2] Compiling Module           ( Module.hs, t/Module.o )
-- [2 of 2] Compiling Module0          ( Module0.hs, t/Module0.o )
--
-- $ ghc --make Module.hs Main.hs
-- [1 of 2] Compiling Module           ( Module.hs, t/Module.o )
-- [2 of 2] Compiling Main             ( Main.hs, t/Main.o )
-- Linking Main ...
--
-- In the first case, the linking step is not performed since there is no module
-- called 'Main'.
--
-- Additionally, the module 'Main' can have an arbitrary source file name, not
-- necessary 'Main.hs'. This changes the name of the output executable:
--
-- $ ghc --make Module.hs MyProg.hs
-- [1 of 2] Compiling Module           ( Module.hs, t/Module.o )
-- [2 of 2] Compiling Main             ( MyProg.hs, t/Main.o )
-- Linking MyProg ...
--
guessOutputFilename :: Maybe FilePath -> [FilePath] -> Maybe FilePath
guessOutputFilename (Just n) _  = Just n
guessOutputFilename Nothing [n] = Just (dropExtension n)
guessOutputFilename Nothing _   = Nothing

fatal :: String -> IO ()
fatal msg = hPutStrLn stderr $ "ghc-parmake: " ++ msg

-- | All flags conflicting with `ghc -M`.
-- Obtained from the man page (listed in the same order as they appear there)
-- and ghc/Main.hs, `data PostLoadMode`:
-- All modes that are not `DoMkDependHS` (`-M`) are conflicting
-- (apart from `--make`).
flagsConflictingWithM :: [String]
flagsConflictingWithM =
  -- "Help and verbosity options"
  [ "?"
  , "--supported-extensions"
  , "--supported-languages"
  , "--info"
  , "--version"
  , "--numeric-version"
  , "--print-libdir"
  -- -V and --help are not included here because this program uses them

  -- "Which phases to run"
  , "-E"
  , "-C"
  , "-S"
  , "-c"

  -- "Alternative modes of operation"
  , "--interactive"
  , "-e"

  -- "Interface file options"
  , "--show-iface"

  -- Undocumented?
  , "--abi-hash"
  ]

-- Program entry point.

main :: IO ()
main =
  do argv <- getArgs
     let args = parseArgs argv
     let (ghcArgs, files) = getGhcArgs argv
     let v = verbosity $ args

     when (printVersion args)   $ putStrLn "ghc-parmake 0.1.6" >> exitSuccess
     when (printUsage args)     $ usage >> exitSuccess

     when (null $ ghcPath args) $ fatal "ghc path is invalid" >> exitFailure

     -- Cases in which we just want to pass on all arguments to GHC and be
     -- as transparent as possible:
     --
     -- * --numeric-version is used
     --   (e.g. cabal does this to determine the GHC version)
     -- * No input files are given
     -- * An option conflicting with "-M" is given
     let passToGhc = exitWith =<<
           runProcess defaultOutputHooks Nothing (ghcPath args)
                                                 (ghcArgs ++ files)

     when (any (`elem` ghcArgs) flagsConflictingWithM) $ passToGhc

     -- We must not print this (or any other output) before handling the
     -- skip-to-GHC cases above.
     debug' v $ "Parsed args: " ++ show args

     when (null files) $ passToGhc

     debug' v "Running ghc -M (twice)..."
     deps <- Parse.getModuleDeps v (ghcPath args) ghcArgs files
     when (null deps) $ do
      hPutStrLn stderr "ghc-parmake: no dependencies"
      exitFailure

     debug' v ("Parsed dependencies:\n" ++ show deps)
     let settings = BuildPlan.Settings { BuildPlan.osuf  = osuf args
                                       , BuildPlan.hisuf = hisuf args }
         plan = BuildPlan.new settings deps (extraDepends args)
     debug' v ("Produced a build plan:\n" ++ show plan)

     debug' v "Building..."
     let ofn = guessOutputFilename (outputFilename args) files
     exitCode <- Engine.compile v plan (numJobs args)
                 (ghcPath args) ghcArgs files ofn
     when (exitCode /= ExitSuccess) $ exitWith exitCode

     unless (skipFinalPass args) $ do
       debug' v $ "Running final ghc --make pass to account for changes ghc -M cannot notice: "
                  ++ ghcPath args ++ " " ++ unwords (ghcArgs ++ files)
       passToGhc -- exits the program
