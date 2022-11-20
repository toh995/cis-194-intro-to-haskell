{-# OPTIONS_GHC -Wall #-}

module Golf where

import qualified Data.Char as Char
import qualified Data.Map as Map

-- Exercise 1
skips :: [a] -> [[a]]
skips xs = map
             (\(n, _) -> everyNth n xs)
             (zip [1..] xs)

everyNth :: Integer -> [a] -> [a]
everyNth n = (map snd)
           . (filter (\(idx, _) -> idx `mod` n == 0))
           . (zip [1..])


-- Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima = map (\(_,y,_) -> y)
            . filter (\(x,y,z) -> y > x && y > z)
            . getTriples

getTriples :: [a] -> [(a, a, a)]
getTriples xs = zip3
                  ((tail.tail) xs)
                  (tail xs)
                  xs


-- Exercise 3
histogram :: [Integer] -> String
histogram = buildDisplay . getCounts

getCounts :: [Integer] -> Map.Map Integer Integer
getCounts = foldr
              (\n accum -> Map.insertWith (+) n 1 accum)
              Map.empty

buildDisplay :: Map.Map Integer Integer -> String
buildDisplay m = unlines
                   ((buildStarLines m) ++ baseLines)
    where baseLines = [
                        ((take 10) . repeat) '=',
                        map Char.intToDigit [0..9]
                      ]

buildStarLines :: Map.Map Integer Integer -> [String]
buildStarLines m = map
                 (`buildStarLine` m)
                 (reverse [1..maxLines])
    where maxLines = (maximum . Map.elems) m

buildStarLine :: Integer -> Map.Map Integer Integer -> String
buildStarLine lineNum m = map
                            (getHistogramChar lineNum m)
                            [0..9]

getHistogramChar :: Integer
                 -> Map.Map Integer Integer
                 -> Integer
                 -> Char
getHistogramChar lineNum m colNum = case (Map.lookup colNum m) of
                                         (Just count) -> if count >= lineNum
                                                         then '*'
                                                         else ' '
                                         (Nothing)    -> ' '
