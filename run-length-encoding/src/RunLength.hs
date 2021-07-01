module RunLength (decode, encode) where

import Data.Char

decode :: String -> String
decode text = concatMap (\(n, c) -> replicate n c) $ group text

group :: String -> [(Int, Char)]
group [] = []
group str = (n, char) : group rest
  where
    n = case digits of
      "" -> 1
      _ -> read digits
    digits = takeWhile isDigit str
    (char : rest) = dropWhile isDigit str

encode :: String -> String
encode text = concatMap runLength $ split text

split :: String -> [String]
split [] = []
split str = takeWhile matchesHead str : split (dropWhile matchesHead str)
  where
    matchesHead :: Char -> Bool
    matchesHead = (== head str)

runLength :: String -> String
runLength s = case length s of
  1 -> [head s]
  _ -> show (length s) ++ [head s]
