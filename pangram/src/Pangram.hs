module Pangram (isPangram) where

import Data.Char
import Data.Map (Map, adjust, fromList)
import qualified Data.Map as Map

cleanMap :: Map Char Int
cleanMap = fromList $ zip ['a' .. 'z'] (repeat 0)

popCharMap :: Map Char Int -> String -> Map Char Int
popCharMap cm [] = cm
popCharMap cm (x : xs) = popCharMap newCm xs
  where
    newCm = adjust (+ 1) x cm

isPangram :: String -> Bool
isPangram text = null filteredMap
  where
    cleanText = map toLower text
    poppedMap = popCharMap cleanMap cleanText
    filteredMap = Map.filter (== 0) poppedMap
