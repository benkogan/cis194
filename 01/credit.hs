-- Name: Ben Kogan

module HW01 where

-- Validate a credit card number (ex. 5)

validate :: Integer -> Bool
validate x = (sumDigits $ doubleEveryOther $ toDigits x) `mod` 10 == 0

-- Convert `Integer` to a list of its digits (ex. 2)

toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0    = []
  | otherwise = toDigits (dropLastDigit x) ++ [lastDigit x]

-- Return last digit of `Integer` (ex. 1)

lastDigit :: Integer -> Integer
lastDigit x = x `mod` 10

-- Return all but last digit of `Integer` (ex. 1)

dropLastDigit :: Integer -> Integer
dropLastDigit x = x `div` 10

-- Double every other digit, starting from rhs (ex. 3)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ doubleEveryOtherLhs $ reverse xs

-- Double every other digit, starting from lhs

doubleEveryOtherLhs :: [Integer] -> [Integer]
doubleEveryOtherLhs []       = []
doubleEveryOtherLhs [x]      = [x]
doubleEveryOtherLhs (x:y:zs) = x : y*2 : doubleEveryOtherLhs zs

-- Sum all digits in a list of Integers (ex. 4)

sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ concat $ map toDigits xs

