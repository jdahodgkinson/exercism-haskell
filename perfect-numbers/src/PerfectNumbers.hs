module PerfectNumbers (classify, Classification (..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n < 1 = Nothing
  | aSum == n = Just Perfect
  | aSum > n = Just Abundant
  | aSum < n = Just Deficient
  | otherwise = Nothing
  where
    aSum = aliquotSum n

aliquotSum :: Int -> Int
aliquotSum n = sum (getFactors n)

getFactors :: Int -> [Int]
getFactors n = filter (`isFactorOf` n) [1 .. (n `div` 2)]
  where
    p `isFactorOf` q = mod q p == 0
