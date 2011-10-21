-- Misc. small useful functions.

module GHC.ParMake.Common (appendMap, maybeRead, pathToMaybe, uniq)
       where

import Data.Maybe (listToMaybe)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

pathToMaybe :: FilePath -> Maybe FilePath
pathToMaybe [] = Nothing
pathToMaybe p  = Just p

-- | Map f over l and then append rest to l. More efficient than doing `map f l
-- ++ rest`.
appendMap :: (t -> a) -> [t] -> [a] -> [a]
appendMap f l rest = go l
  where
    go [] = rest
    go (x:xs) = f x : go xs

-- | Remove consecutive duplicate elements from a list.
uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq [x] = [x]
uniq (x1:x2:xs) = let rest = x2:xs
                  in if x1 == x2 then uniq rest else x1 : uniq rest
