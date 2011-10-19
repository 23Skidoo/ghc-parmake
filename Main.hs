module Main
       where

import System.Environment (getArgs)

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

getGhcArgs :: [String] -> [String]
getGhcArgs []          = []
getGhcArgs ("-j":_:xs) = xs
getGhcArgs (x:xs)      = x:(getGhcArgs xs)

-- Program entry point.

main :: IO ()
main = do args <- getArgs
          let numJobs = getNumJobs args
          let ghcArgs = getGhcArgs args
          putStrLn $ "Num jobs: " ++ show numJobs
          putStrLn $ "GHC args: " ++ show ghcArgs
          putStrLn "Trying to parse module dependency information..."
          ds <- Parse.getModuleDeps ghcArgs
          let plan = BuildPlan.new ds
          print plan
          Engine.compileInParallel plan
