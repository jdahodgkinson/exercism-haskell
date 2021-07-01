module Diamond (diamond) where

import Data.Char

diamond :: Char -> Maybe [String]
diamond c
  | not $ isAlpha c = Nothing
  | otherwise = Just (map processRow $ rows $ toUpper c)

processRow :: (Char, Int, Int) -> String
processRow (c, x, y) = mirror $ replicate x ' ' ++ [c] ++ replicate y ' '

rows :: Char -> [(Char, Int, Int)]
rows c = mirror (zip3 chars spaceBefore spaceAfter)
  where
    chars = ['A' .. c]
    len = length chars
    spaceAfter = [0 .. len -1]
    spaceBefore = reverse spaceAfter

mirror :: [a] -> [a]
mirror xs = xs ++ tail (reverse xs)
