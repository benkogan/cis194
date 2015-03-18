{-# OPTIONS_GHC -Wall #-}
module Golf where

import Data.List

-- | Exercise 1

skips :: [a] -> [[a]]
skips [] = []
skips xs = map (`every` xs) [1..(length xs)]
  where every i ys = drop (i-1) (take i ys) ++ every i (drop i ys)

-- | Exercise 2

localMaxima :: [Integer] -> [Integer]
localMaxima = (map maximum) . part

part :: [a] -> [[a]]
part [] = []
part xs = take 3 xs : part (drop 3 xs)

-- | Exercise 3

histogram :: [Integer] -> String
histogram xs = put starCount ++ "\n==========\n0123456789\n"
  where put = concat . map ((++) "\n") . transpose . map stars
        starCount = map (count xs) [0..9]
        count ys a = length $ filter (\x -> x == a) ys
        stars n = replicate (maxi - n) ' ' ++ replicate n '*'
        maxi = maximum starCount

