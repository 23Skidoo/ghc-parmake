module Main
       where

import GHC.ParMake.Common (appendMap)

import Test.QuickCheck
import Test.Framework (Test, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)

pAppendMap :: [Int] -> [Int] -> Bool
pAppendMap l1 l2 = appendMap id l1 l2 == l1 ++ l2

------------------------------------------------------------------------
-- Test harness

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testProperty "appendMap" pAppendMap
    -- , testGroup "text"
    --   [ testProperty "text/strict" pText
    --   , testProperty "text/lazy" pTextLazy
    --   , testProperty "rechunk" pRechunk
    --   , testProperty "text/rechunked" pLazyRechunked
    --   ]
    ]
