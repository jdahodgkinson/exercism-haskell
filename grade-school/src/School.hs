module School (School, add, empty, grade, sorted) where

import Data.Bifunctor
import Data.List

type Grade = (Int, [String])

type School = [Grade]

add :: Int -> String -> School -> School
add gradeNum student school = sorted $ (gradeNum, students) : rest
  where
    rest :: [Grade]
    rest = filter (\(n, _) -> n /= gradeNum) school
    students :: [String]
    students = student : grade gradeNum school

empty :: School
empty = []

grade :: Int -> School -> [String]
grade gradeNum school
  | any matchesGrade school = snd $ head $ filter matchesGrade school
  | otherwise = []
  where
    matchesGrade :: Grade -> Bool
    matchesGrade g = gradeNum == fst g

sorted :: School -> [(Int, [String])]
sorted school = sort $ map (second sort) school
