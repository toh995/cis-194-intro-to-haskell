toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = (n `rem` 10) : toDigitsRev (n `quot` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse.doubleEveryOtherFromLeft.reverse

doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft []         = []
doubleEveryOtherFromLeft [d]        = [d]
doubleEveryOtherFromLeft (d1:d2:ds) = d1 : (2*d2) : doubleEveryOtherFromLeft ds

sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (d:ds) = (sum (toDigits d)) + (sumDigits ds)

computeChecksum :: Integer -> Integer
computeChecksum = sumDigits.doubleEveryOther.toDigits

validate :: Integer -> Bool
validate n = (computeChecksum n `rem` 10) == 0
