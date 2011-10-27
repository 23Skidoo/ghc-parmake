module B
       where

import {-# SOURCE #-} qualified A

exportedString :: String
exportedString = "test string"

bString :: String
bString = A.exportedString
