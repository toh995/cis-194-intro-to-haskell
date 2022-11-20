{-# OPTIONS_GHC -Wall #-}

import Data.List ((\\))

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

-- Exercise 1 - rewrite fun1 and fun2
fun1' :: [Integer] -> Integer
fun1' = product
      . map (subtract 2)
      . filter even

fun2' :: Integer -> Integer
fun2' = sum
      . filter even
      . takeWhile (/= 1)
      . iterate f 
    where f n
            | even n    = n `div` 2
            | otherwise = 3 * n + 1

-- Exercise 2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert x Leaf                 = Node 0 Leaf x Leaf
insert x (Node _ l currVal r) =
  Node newHeight newLeft currVal newRight
    where newLeft   = if (height l <= height r) then (insert x l) else l
          newRight  = if (height l >  height r) then (insert x r) else r
          newHeight = 1 + max (height newLeft) (height newRight)

height :: Tree a -> Integer
height Leaf           = 0
height (Node h _ _ _) = h

-- Exercise 3
xor :: [Bool] -> Bool
xor = odd
    . foldr (\_ acc -> acc + 1 :: Integer) 0
    . filter id

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> (f x) : acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (flip f) base
               . reverse

-- Exercise 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (+1)
                . map (*2)
                . ([1..n] \\) 
                . filter (<= n)
                . map (\(i,j) -> i + j + 2*i*j)
                . filter (\(i,j) -> i <= j)
                $ cartProd [1..n] [1..n]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
