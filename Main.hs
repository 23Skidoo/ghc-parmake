module Main
       where

import Control.Monad (liftM, when)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess, exitWith)
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
  outputFilename :: Maybe String
  } deriving Show

defaultArgs :: Args
defaultArgs = Args {
  verbosity      = normal,
  printVersion   = False,
  printUsage     = False,
  numJobs        = 1,
  outputFilename = Nothing
  }

parseArgs :: [String] -> Args
parseArgs l = go l defaultArgs
  where
    parseNumJobs n = fromMaybe (error "The argument to '-j' must be an integer!")
                     (liftM abs $ maybeRead n)
    parseVerbosity n = fromMaybe verbose (maybeRead n >>= intToVerbosity)

    go [] acc                      = acc
    go ("-V":_) acc                = acc { printVersion = True }
    go ("--help":_) acc            = acc { printUsage = True }
    go ("-j":n:as) acc             = go as $ acc { numJobs = parseNumJobs n }
    go (('-':'j':n:[]):as) acc     = go as $ acc { numJobs = parseNumJobs [n] }
    go (('-':'v':n:[]):as) acc     = go as $
                                     acc { verbosity = parseVerbosity [n] }
    go (('-':'v':'v':n:[]):as) acc = go as $
                                     acc { verbosity = parseVerbosity [n] }
    go ("-v":as) acc               = go as $ acc { verbosity = verbose }
    go ("-o":n:as) acc             = go as $ acc { outputFilename = Just n }
    go (_:as) acc                  = go as acc


getGhcArgs :: [String] -> ([String],[String])
getGhcArgs argv = let (as, fs) = getGhcArgs' argv [] []
                  in (reverse as, reverse fs)
  where
    pgmSuffixes =  ["L", "P", "c", "m", "s", "a", "l", "dll", "F", "windres"]
    optsWithArg = [ "-odir", "-hidir", "-ohi", "-stubdir", "-outputdir"
                  , "-tmpdir", "-osuf", "-hisuf", "-hcsuf"
                  , "-package", "-package-id", "-hide-package", "-ignore-package"
                  , "-package-name", "-package-conf", "-f"
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
    -- Options not passed to GHC: -o, -j, -vv.
    getGhcArgs' ("-j":_:xs) as fs             = getGhcArgs' xs as fs
    getGhcArgs' ("-o":_:xs) as fs             = getGhcArgs' xs as fs
    getGhcArgs' (('-':'v':'v':_:[]):xs) as fs = getGhcArgs' xs as fs
    getGhcArgs' xs@(('-':_):_) as fs          = let (xs', as') = eatOption xs as
                                                in getGhcArgs' xs' as' fs
    getGhcArgs' (x:xs) as fs                  = getGhcArgs' xs as (x:fs)

usage :: IO ()
usage =
  putStr $ "Usage: ghc-parmake [OPTIONS] FILES\n" ++
  "A parallel wrapper around 'ghc --make'.\n\n" ++
  "Options: \n" ++
  "-j N    - Run N jobs in parallel. \n" ++
  "-vv[N]  - Set verbosity to N (only for ghc-parmake). " ++
  "N is 0-3, default 1.\n" ++
  "-v[N]   - Set verbosity to N (both for GHC and ghc-parmake itself). \n" ++
  "--help  - Print usage information. \n" ++
  "-V      - Print version information. \n" ++
  "\nOther options are passed to GHC unmodified.\n"

guessOutputFilename :: Maybe FilePath -> [FilePath] -> FilePath
guessOutputFilename (Just n) _  = n
guessOutputFilename Nothing [n] = dropExtension n
guessOutputFilename Nothing _   = "a.out"

noInputFiles :: IO ()
noInputFiles = hPutStrLn stderr "ghc-parmake: no input files"

-- Program entry point.

main :: IO ()
main =
  do argv <- getArgs
     let args = parseArgs argv
     let (ghcArgs, files) = getGhcArgs argv
     let v = verbosity $ args
     debug' v $ "Parsed args: " ++ show args

     when (printVersion args) $ putStrLn "ghc-parmake 0.1" >> exitSuccess
     when (printUsage args)   $ usage >> exitSuccess
     when (null files)        $ noInputFiles >> exitFailure

     debug' v "Running ghc -M..."
     deps <- Parse.getModuleDeps ghcArgs files
     when (null deps) $ exitFailure

     debug' v ("Parsed dependencies:\n" ++ show deps)
     let plan = BuildPlan.new deps
     debug' v ("Produced a build plan:\n" ++ show plan)

     debug' v "Building..."
     let ofn = guessOutputFilename (outputFilename args) files
     exitCode <- Engine.compile v plan (numJobs args) ghcArgs files ofn
     exitWith exitCode
