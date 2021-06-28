module Acronym (abbreviate) where

import Data.Char

abbreviate :: String -> String
abbreviate xs = concatMap initialise $ splitWords xs

splitWords :: String -> [String]
splitWords [] = []
splitWords xs = firstWord : splitWords formatRemWords
  where
    isNotSeperator = \x -> x `notElem` [' ', '-']
    firstWord = takeWhile isNotSeperator xs
    remainingWords = dropWhile isNotSeperator xs
    formatRemWords
      | null remainingWords = []
      | otherwise = tail remainingWords

initialise :: String -> String
initialise "" = ""
initialise x
  | allCaps || noCaps = [toUpper (head x)]
  | otherwise = filter isUpper x
  where
    allCaps = all isUpper x
    noCaps = all isLower x
