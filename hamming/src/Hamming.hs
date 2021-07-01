module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance [] [] = Just 0
distance _ [] = Nothing
distance [] _ = Nothing
distance (x : xs) (y : ys)
  | x == y = distance xs ys
  | otherwise = distance xs ys >>= (\n -> Just (n + 1))
