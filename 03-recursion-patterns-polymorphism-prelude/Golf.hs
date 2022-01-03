module Golf where

import Data.List (intercalate, replicate)

-- Exercise 1 - Hopscotch
-- Use a nested list comprehension
-- n represents how large of a "step" to take
-- i represents the indexes based on the current "n" step
skips :: [a] -> [[a]]
skips l = [[l!!i | i<-[n-1, 2*n-1 .. length l - 1]] | n<-[1..length l]]

-- Exercise 2 - Local maxima
-- Compare each trio of integers
-- If we have a local max, then prepend to the list
-- Define a function `g` to shorten the recursive calls
-- Use `True` instead of `otherwise` within the guard (it's shorter :P)
localMaxima :: [Integer] -> [Integer]
localMaxima = g

g :: [Integer] -> [Integer]
g (x:y:z:l)
  | y > x && y > z = y : g (y:z:l)
  | True = g (y:z:l)
g _ = []

-- Exercise 3 - Histogram
-- The code for this exercise is more verbose, to improve readability
histogram :: [Integer] -> String
histogram vals = intercalate "\n" lines
  where lines = buildMainLines vals ++ [replicate 10 '=', "0123456789"]

buildMainLines :: [Integer] -> [String]
buildMainLines vals = map (buildMainLine freqs) (reverse [1..maximum freqs])
  where freqs = frequencies vals

buildMainLine :: [Int] -> Int -> String
buildMainLine freqs currFreq = map f freqs
  where f freq
          | freq >= currFreq = '*'
          | otherwise        = ' '

frequencies :: [Integer] -> [Int]
frequencies vals = [length (filter (== i) vals) | i<-[0..9]]
