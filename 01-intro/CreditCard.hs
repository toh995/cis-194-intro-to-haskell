{-# OPTIONS_GHC -Wall #-}

-- EXERCISE 1
toDigits :: Integer -> [Integer]
toDigits = myReverse.toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | n < 10    = [n]
  | otherwise = (n `mod` 10) : (toDigitsRev (n `div` 10))

-- implementing my own reverse algo for practice
-- this is probably not very time-efficient...
myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = (myReverse (tail xs)) ++ [head xs]

-- EXERCISE 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse.doubleEveryOtherFromLeft.reverse

doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft [] = []
doubleEveryOtherFromLeft [x] = [x]
doubleEveryOtherFromLeft (x:y:zs) = x : (2*y) : (doubleEveryOtherFromLeft zs)

-- EXERCISE 3
sumDigits :: [Integer] -> Integer
sumDigits = sum.concat.(map toDigits)

-- EXERCISE 4
validate :: Integer -> Bool
validate n = (computeChecksum n) `mod` 10 == 0

computeChecksum :: Integer -> Integer
computeChecksum = sumDigits.doubleEveryOther.toDigits
