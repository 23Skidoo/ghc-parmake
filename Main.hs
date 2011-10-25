module Main
       where

import System.Environment (getArgs)
import System.Exit (exitWith)

import GHC.ParMake.Common (maybeRead)

import qualified GHC.ParMake.BuildPlan as BuildPlan
import qualified GHC.ParMake.Parse as Parse
import qualified GHC.ParMake.Engine as Engine

-- Argument handling.

getNumJobs :: [String] -> Int
getNumJobs []         = 1
getNumJobs ("-j":n:_) =
  case maybeRead n of
    Just n' -> abs n'
    Nothing -> error "The argument to '-j' must be an integer!"
getNumJobs (_:xs)     = getNumJobs xs

getGhcArgs :: [String] -> ([String],[String])
getGhcArgs argv = let (as, fs) = getGhcArgs' argv [] []
                  in (reverse as, reverse fs)
  where
    getGhcArgs' [] as fs             = (as, fs)
    getGhcArgs' ("-j":_:xs) as fs    = getGhcArgs' xs as fs
    getGhcArgs' (x@('-':_):xs) as fs = getGhcArgs' xs (x:as) fs
    getGhcArgs' (x:xs) as fs         = getGhcArgs' xs as (x:fs)

-- Program entry point.

main :: IO ()
main = do args <- getArgs
          let numJobs = getNumJobs args
          let (ghcArgs, files) = getGhcArgs args
          plan <- BuildPlan.new `fmap` Parse.getModuleDeps ghcArgs files
          exitCode <- Engine.compile plan numJobs ghcArgs
          exitWith exitCode
