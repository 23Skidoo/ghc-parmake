module Main
       where

import Data.Char
import Data.List
import System.FilePath
import Test.QuickCheck
import Test.Framework (Test, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified GHC.ParMake.BuildPlan as BuildPlan
import GHC.ParMake.BuildPlan (TargetId)
import GHC.ParMake.Common (appendMap, uniq)

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
                                  && (isUpper . head $ l)
                           )

pMarkCompleted :: DepsList -> Bool
pMarkCompleted (DepsList []) = True
pMarkCompleted (DepsList  l) =
  let plan = BuildPlan.new l
      target = head . BuildPlan.ready $ plan
      plan' = BuildPlan.markReadyAsBuilding plan
      plan'' = BuildPlan.markCompleted plan' target
  in target `elem` BuildPlan.completed plan''

pCompile :: DepsList -> Bool
pCompile (DepsList []) = True
pCompile (DepsList l) = go $ BuildPlan.new l
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
-- Test harness

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testProperty "appendMap" pAppendMap
    , testProperty "markCompleted" pMarkCompleted
    , testProperty "compile" pCompile
    -- , testGroup "text"
    --   [ testProperty "text/strict" pText
    --   , testProperty "text/lazy" pTextLazy
    --   , testProperty "rechunk" pRechunk
    --   , testProperty "text/rechunked" pLazyRechunked
    --   ]
    ]
