module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year
  -- No number not evenly divisible by four is a leap year.
  | not evenDivBy4 = False
  -- Any number evenly divisible by 100 but not 400 is a leap year.
  | evenDivBy100 && not evenDivBy400 = False
  -- All other numbers are leap years.
  | otherwise = True
  where
    evenDivBy4 = rem year 4 == 0
    evenDivBy100 = rem year 100 == 0
    evenDivBy400 = rem year 400 == 0
