module Phone (number) where

import Data.Char

number :: String -> Maybe String
number xs =
  allowedChars xs
    >>= clean
    >>= trunc
    >>= validDigits

allowedChars :: String -> Maybe String
allowedChars s
  | all allowed s = Just s
  | otherwise = Nothing
  where
    allowed :: Char -> Bool
    allowed c = isDigit c || elem c ['+', '-', '.', ' ', '(', ')']

clean :: String -> Maybe String
clean s = Just (filter isDigit s)

trunc :: String -> Maybe String
trunc s
  | length s == 10 = Just s
  | length s == 11 && head s == '1' = Just (tail s)
  | otherwise = Nothing

validDigits :: String -> Maybe String
validDigits s
  | validAreaCode && validExchangeCode = Just s
  | otherwise = Nothing
  where
    validAreaCode = head s `elem` ['2' .. '9']
    validExchangeCode = (s !! 3) `elem` ['2' .. '9']
