module Main
       where

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
  -- Take a random subset of program modules.
  do ifcs' <- listOf $ elements fns
     let ifcs = uniq . sort $ ifcs'
     -- Generate deps for this file.
     let fnExt = fn <.> "o"
     let deps = (fnExt, fn <.> "hs") : map (\i -> (fnExt, i <.> "hi")) ifcs
     -- Generate a deps for the remaining modules.
     rest <- (mkObjsDeps fns)
     return $ deps ++ rest

arbitraryDepsList :: Gen [(TargetId, TargetId)]
arbitraryDepsList = do fns' <- listOf arbitraryName `suchThat` ((<=) 5 . length)
                       let fns = uniq . sort $ fns'
                       mkObjsDeps fns

arbitraryName :: Gen TargetId
arbitraryName = fmap (:[]) $ elements ['A'..'Z']

pMarkCompleted :: DepsList -> Bool
pMarkCompleted (DepsList []) = True
pMarkCompleted (DepsList  l) =
  let plan = BuildPlan.new l
      target = head . BuildPlan.ready $ plan
      plan' = BuildPlan.markReadyAsBuilding plan
      plan'' = BuildPlan.markCompleted plan' target
  in target `elem` BuildPlan.completed plan''

-- arbitraryName :: Gen TargetId
-- arbitraryName = (listOf . elements $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'])
--                 `suchThat` (\l -> (not . null $ l)
--                                   && (not . isDigit . head $ l)
--                                   && (length l >= 4)
--                                   && (isUpper . head $ l)
--                            )

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
    -- , testGroup "text"
    --   [ testProperty "text/strict" pText
    --   , testProperty "text/lazy" pTextLazy
    --   , testProperty "rechunk" pRechunk
    --   , testProperty "text/rechunked" pLazyRechunked
    --   ]
    ]
