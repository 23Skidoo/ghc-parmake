module Main
       where

import Control.Monad
import Control.Exception
import Data.Char
import Data.List
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process

import Test.QuickCheck
import Test.HUnit hiding (Test)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import qualified GHC.ParMake.BuildPlan as BuildPlan
import GHC.ParMake.BuildPlan (TargetId, defaultSettings)
import GHC.ParMake.Common (appendMap, uniq)
import GHC.ParMake.Parse (depsListToDeps)

------------------------------------------------------------------------
-- Input data generation for QuickCheck.

data DepsList = DepsList [(TargetId, TargetId)]
              deriving Show

instance Arbitrary DepsList where
  arbitrary = fmap DepsList arbitraryDepsList

mkObjsDeps :: [TargetId] -> Gen [(TargetId, TargetId)]
mkObjsDeps []       = return []
mkObjsDeps [fn]     = return [(fn <.> "o", fn <.> "hs")]
mkObjsDeps (fn:fns) =
  -- Choose a random subset of program modules to be used as dependencies for
  -- this file.
  do ifcs' <- listOf $ elements fns
     let ifcs = uniq . sort $ ifcs'
     -- Generate a dep list for this file: each .o depends on a .hs plus a
     -- number of ".hi"s chosen in the previous step.
     let fnExt = fn <.> "o"
     let deps = (fnExt, fn <.> "hs") : map (\i -> (fnExt, i <.> "hi")) ifcs
     -- Generate dep lists for the remaining modules.
     rest <- (mkObjsDeps fns)
     return $ deps ++ rest

arbitraryDepsList :: Gen [(TargetId, TargetId)]
arbitraryDepsList = do fns' <- listOf arbitraryName `suchThat` ((<=) 5 . length)
                       let fns = uniq . sort $ fns'
                       mkObjsDeps fns

-- A simpler name generator.
-- arbitraryName :: Gen TargetId
-- arbitraryName = fmap (:[]) $ elements ['A'..'Z']

arbitraryName :: Gen TargetId
arbitraryName = (listOf . elements $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'])
                `suchThat` (\l -> (not . null $ l)
                                  && (not . isDigit . head $ l)
                                  && (length l >= 4)
                                  && (isUpper . head $ l))

------------------------------------------------------------------------
-- Properties.

pMarkCompleted :: DepsList -> Bool
pMarkCompleted (DepsList []) = True
pMarkCompleted (DepsList  l) =
  let plan = BuildPlan.new defaultSettings (depsListToDeps l) []
      target = head . BuildPlan.ready $ plan
      plan' = BuildPlan.markReadyAsBuilding plan
      plan'' = BuildPlan.markCompleted plan' target
  in [target] == BuildPlan.completed plan''

pCompile :: DepsList -> Bool
pCompile (DepsList []) = True
pCompile (DepsList l) = go $ BuildPlan.new defaultSettings (depsListToDeps l) []
  where
    go plan = if null rdy then check plan else go plan''
      where
        rdy = BuildPlan.ready plan
        plan' = BuildPlan.markReadyAsBuilding plan
        plan'' = foldr (flip BuildPlan.markCompleted) plan' rdy
        check p = BuildPlan.numBuilding p == 0
                  && (length $ BuildPlan.completed p) == BuildPlan.size p

pAppendMap :: [Int] -> [Int] -> Bool
pAppendMap l1 l2 = appendMap id l1 l2 == l1 ++ l2

------------------------------------------------------------------------
-- Unit tests.

makeProgram :: String
makeProgram = "dist" </> "build" </> "ghc-parmake" </> "ghc-parmake"

recreateDirectory :: FilePath -> IO ()
recreateDirectory dir =
  do dirExists <- doesDirectoryExist dir
     when dirExists $ removeDirectoryRecursive dir
     createDirectory dir

getExitCode :: FilePath -> [String] -> FilePath -> IO ExitCode
getExitCode program args workingDir =
  do bracket (runInteractiveProcess program args (Just workingDir) Nothing)
       (\(inh, outh, errh, _) -> mapM_ hClose [inh, outh, errh])
       (\(_,_,errh,pid) -> do exitCode <- waitForProcess pid
                              hGetContents errh >>= putStrLn
                              return exitCode)

mkTestCase :: FilePath -> Int -> Assertion
mkTestCase dirName numJobs =
  do t1 <- doesDirectoryExist testDir
     t2 <- doesFileExist makeProgram
     assertBool ("Directory '" ++ testDir ++ "' doesn't exist!") t1
     assertBool ("Executable '" ++ makeProgram ++ "' doesn't exist!") t2

     -- Try building the program with different options.
     buildProgram []
     buildProgram ["-hisuf", "hi_p", "-osuf", "o_p"]

     -- Check output.
     testProgramOutput <- readProcess testProgram [] ""
     referenceOutput <- readFile testFile
     assertEqual "Program output is wrong!" referenceOutput testProgramOutput
       `finally` removeFile testProgram

  where
    buildProgram extraOpts = do
      curDir <- getCurrentDirectory
      recreateDirectory oDir
      let args = [ "Main.hs", "-package", "base", "-j", show numJobs
                 , "-odir", oDirName, "-hidir", oDirName ] ++ extraOpts
      exitCode <- getExitCode (curDir </> makeProgram) args testDir
      assertEqual "ghc-parmake invocation failed!" ExitSuccess exitCode
        `finally` removeDirectoryRecursive oDir

    testDir     = "tests" </> "data" </> dirName
    oDirName    = "tmp"
    oDir        = testDir </> oDirName
    testProgram = testDir </> "Main"
    testFile    = testDir </> "OUTPUT"

-- | Checks that parmake does not create another executable next to
-- the source file if the -o option is given.
testSuperfluousExe :: Assertion
testSuperfluousExe =
  do curDir <- getCurrentDirectory
     let testDir     = "tests" </> "data" </> "output-target"
         oDir        = testDir </> "tmp"
         testProgram = "tmp" </> "myprogram"
         args        =  [ "--make", "Prog.hs", "-o", testProgram ]
         badExe      = testDir </> "Prog"

     recreateDirectory oDir

     -- Remove potentially existing old bad executable
     progExists <- doesFileExist badExe
     when progExists $ removeFile badExe

     exitCode <- getExitCode (curDir </> makeProgram) args testDir

     (do assertEqual "ghc-parmake invocation failed!" ExitSuccess exitCode
         progExist <- doesFileExist (testDir </> testProgram)
         assertBool "target specified via -o was not created" progExist
         badExist <- doesFileExist badExe
         assertBool "executable was created next to source file (should not)"
                    (not badExist)
       ) `finally` removeDirectoryRecursive oDir


------------------------------------------------------------------------
-- Test harness

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "properties"
      [ testProperty "appendMap" pAppendMap
      , testProperty "markCompleted" pMarkCompleted
      , testProperty "compile" pCompile
      ]
    , testGroup "tests"
      [ testCase dirName (mkTestCase dirName 2)
      | dirName <- [ "executable" , "executable-lhs"
                   , "executable-mutrec", "executable-lhs-mutrec" ]
      ]
    , testGroup "custom tests"
      [ testCase "-o does not create superfluous exe" testSuperfluousExe
      ]
    ]
