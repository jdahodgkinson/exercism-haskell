module Isogram (isIsogram) where

import Data.Char

isIsogram :: String -> Bool
isIsogram [] = True
isIsogram (c : cs) = case c of
  ' ' -> isIsogram cs
  '-' -> isIsogram cs
  _ -> d `notElem` ds && isIsogram ds
    where
      d = toLower c
      ds = map toLower cs
