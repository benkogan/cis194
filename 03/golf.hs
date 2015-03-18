{-# OPTIONS_GHC -Wall #-}
module Golf where

import Data.List

-- | Exercise 1
-- `every` returns every `i`th element from `ys`

skips :: [a] -> [[a]]
skips [] = []
skips xs = map (`every` xs) [1..(length xs)]
  where every i ys = drop (i-1) (take i ys) ++ every i (drop i ys)

-- | Exercise 2
-- `part` partitions list `xs` into a list of sublists of length 3

localMaxima :: [Integer] -> [Integer]
localMaxima = (map maximum) . part

part :: [a] -> [[a]]
part [] = []
part xs = take 3 xs : part (drop 3 xs)

-- | Exercise 3

histogram :: [Integer] -> String
histogram = error "TODO"

count :: Ord a => [a] -> [(a, Int)]
count = map (\xs@(x:_) -> (x, length xs)) . group . sort

