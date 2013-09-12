module Main
       where

import Control.Monad (liftM, when)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure, exitSuccess, exitWith)
import System.IO (hPutStrLn, stderr, hSetBuffering, BufferMode(LineBuffering))

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
  hisuf          :: String
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
  hisuf          = "hi"
  }

parseArgs :: [String] -> Args
parseArgs l = go l defaultArgs
  where
    parseNumJobs n = fromMaybe (fatal "The argument to '-j' must be an integer!")
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
    go ("-optP-include":as) acc      = handleOptPInclude as acc
    go ("-o":n:as) acc               = go as $ acc { outputFilename = Just n }
    go ("-osuf":suf:as) acc          = go as $ acc { osuf = suf }
    go ("-hisuf":suf:as) acc         = go as $ acc { hisuf = suf }
    go ("--ghc-path":p:as) acc       = go as $ acc { ghcPath = p }
    go (a:as) acc
      | "--ghc-path=" `isPrefixOf` a = let (o,p') = break (== '=') a in
                                       go (o:(tail p'):as) acc
    go (_:as) acc                    = go as acc


    -- Add '-optP-include -optPmyfile' as extraDepends
    handleOptPInclude [] _ = fatal "no path is given after -optP-include"
    handleOptPInclude (optPfile:as) acc@Args { extraDepends = ds } =
      case splitOffPrefix "-optP" optPfile of
        Just path | not (null path) -> go as $ acc { extraDepends = path : ds }
                  | otherwise       -> fatal $
                                       "path given after -optP-include is empty"
        _                           -> fatal "missing -optP after -optP-include"


splitOffPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
splitOffPrefix p s = case splitAt (length p) s of
  (p', r) | p' == p -> Just r
  _                 -> Nothing


-- | Processes a list of arguments, returning:
-- * the GHC arguments we want to use in parmake
-- * the files to be compiled
-- * the original GHC arguments with custom parmake arguments removed
--   (thus also contains files)
getGhcArgs :: [String] -> ([String],[String],[String])
getGhcArgs argv = let nonParmakeArgs = rmArgs argv
                      (args, files) = mkArgs nonParmakeArgs [] []
                  in (args, files, nonParmakeArgs)
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

    -- Processes GHC args to create GHC style args suiting for parmake,
    -- and splitting files apart.
    mkArgs [] as fs                     = (reverse as, reverse fs)
    mkArgs ("-o":_:xs) as fs            = mkArgs xs as fs
    mkArgs ("--make":xs) as fs          = mkArgs xs as fs
    mkArgs xs@(('-':_):_) as fs         = let (xs', as') = eatOption xs as
                                          in mkArgs xs' as' fs
    mkArgs (x:xs) as fs                 = mkArgs xs as (x:fs)

    -- Removes parmake args from a list of arguments.
    rmArgs []                           = []
    -- Options not passed to GHC: -o, -j, -vv, --ghc-path, --make.
    rmArgs ("-j":_:xs)                  = rmArgs xs
    rmArgs (('-':'v':'v':_:[]):xs)      = rmArgs xs
    rmArgs ("--ghc-path":_:xs)          = rmArgs xs
    rmArgs (x:xs)
      | "--ghc-path=" `isPrefixOf` x    = rmArgs xs
    rmArgs (arg:xs)                     = arg : rmArgs xs


usage :: IO ()
usage =
  putStr $ "Usage: ghc-parmake [OPTIONS] FILES\n" ++
  "A parallel wrapper around 'ghc --make'.\n\n" ++
  "Options: \n" ++
  "-j N             - Run N jobs in parallel.\n" ++
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
-- We currently solve this problem by the final real GHC pass.

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
  do -- Set stderr to line buffering to prevent interleaved GHC errors
     hSetBuffering stderr LineBuffering

     argv <- getArgs
     let args = parseArgs argv
     let (parmakeGhcArgs, files, nonParmakeArgs) = getGhcArgs argv
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
           runProcess defaultOutputHooks Nothing (ghcPath args) nonParmakeArgs

     when (any (`elem` parmakeGhcArgs) flagsConflictingWithM) $ passToGhc

     -- We must not print this (or any other output) before handling the
     -- skip-to-GHC cases above.
     debug' v $ "Parsed args: " ++ show args

     when (null files) $ passToGhc

     debug' v "Running ghc -M (twice)..."
     deps <- Parse.getModuleDeps v (ghcPath args) parmakeGhcArgs files
     when (null deps) $ do
      hPutStrLn stderr "ghc-parmake: no dependencies"
      exitFailure

     debug' v ("Parsed dependencies:\n" ++ show deps)
     let settings = BuildPlan.Settings { BuildPlan.osuf  = osuf args
                                       , BuildPlan.hisuf = hisuf args }
         plan = BuildPlan.new settings deps (extraDepends args)
     debug' v ("Produced a build plan:\n" ++ show plan)

     debug' v "Building..."
     exitCode <- Engine.compile v plan (numJobs args)
                 (ghcPath args) parmakeGhcArgs files (outputFilename args)
     when (exitCode /= ExitSuccess) $ exitWith exitCode

     debug' v $ "Running final 'ghc --make' pass: "
       ++ ghcPath args ++ " " ++ unwords nonParmakeArgs
     passToGhc
