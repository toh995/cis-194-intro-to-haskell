{-# OPTIONS_GHC -Wall #-}

import Data.List (sortBy)

-- Exercise 1, 2
length' :: [a] -> Integer
length' []     = 0
length' (_:xs) = 1 + (length' xs)

-- Exercise 3
mean :: [Double] -> Double
mean xs = (sum xs) / ((fromIntegral.length) xs)

-- Exercise 4
palindrome :: [a] -> [a]
palindrome xs = xs ++ (reverse xs)

-- Exercise 5
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = (take halfLen xs) == (take halfLen (reverse xs))
  where halfLen = (length xs) `div` 2

-- Exercise 6
sortByLength :: [[a]] -> [[a]]
sortByLength = sortBy compareByLength

compareByLength :: [a] -> [a] -> Ordering
compareByLength xs ys
    | lenX < lenY = LT
    | lenX > lenY = GT
    | otherwise   = EQ
  where lenX = length xs
        lenY = length ys

-- Exercise 7
intersperse :: a -> [[a]] -> [a]
intersperse _   []     = []
intersperse _   (x:[]) = x
intersperse sep (x:xs) = x ++ [sep] ++ (intersperse sep xs)

-- Exercise 8
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

treeHeight :: Tree a -> Integer
treeHeight Empty               = 0
treeHeight (Node _ left right) = 1 + (max (treeHeight left) (treeHeight right))

-- Exercise 9
data Direction = L
               | R
               | Straight
                 deriving (Show)

-- Exercise 10
type Point = (Integer, Integer)

getDirection :: Point -> Point -> Point -> Direction
getDirection p1 p2 p3
    | zCrossProduct > 0 = L
    | zCrossProduct < 0 = R
    | otherwise         = Straight
  where zCrossProduct = getZCrossProduct p1 p2 p3

getZCrossProduct :: Point -> Point -> Point -> Integer
getZCrossProduct (x1, y1) (x2, y2) (x3, y3) = (x2-x1)*(y3-y2) - (y2-y1)*(x3-x2)

-- Exercise 11
getTripleDirections :: [Point] -> [Direction]
getTripleDirections (a:b:c:xs) = (getDirection a b c) : (getTripleDirections (b:c:xs))
getTripleDirections _          = []
