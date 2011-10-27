module Main
       where

import A
import B
import qualified C.C as C

main :: IO ()
main = print aString >> print bString >> C.cString
