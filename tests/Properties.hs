module Main
       where

import GHC.ParMake.Common (mapAppend)

import Test.QuickCheck
import Test.Framework (Test, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)

pMapAppend :: [Int] -> [Int] -> Bool
pMapAppend l1 l2 = mapAppend id l1 l2 == l1 ++ l2

------------------------------------------------------------------------
-- Test harness

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testProperty "mapAppend" pMapAppend
    -- , testGroup "text"
    --   [ testProperty "text/strict" pText
    --   , testProperty "text/lazy" pTextLazy
    --   , testProperty "rechunk" pRechunk
    --   , testProperty "text/rechunked" pLazyRechunked
    --   ]
    ]
