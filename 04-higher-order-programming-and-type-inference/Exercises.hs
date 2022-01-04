import Data.List ((\\))

-- Exercise 1
fun1 :: [Integer] -> Integer
fun1 []       = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product
      . map (subtract 2)
      . filter even

fun2 :: Integer -> Integer
fun2 1        = 0
fun2 n
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum
      . filter even
      . takeWhile (/= 1)
      . iterate (\n -> if even n
                          then n `div` 2
                          else 3 * n + 1)


-- Exercise 2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

{-
Build a new tree from the input tree, such that:
  (1) The given element is inserted into the tree
  (2) The new tree is balanced

Assume that the input tree is already balanced...
-}
insert :: a -> Tree a -> Tree a
insert x Leaf                   = Node 0 Leaf x Leaf
insert x (Node _ lTree y rTree) = Node newHeight newLTree y newRTree
        where
          newLTree
            | getHeight lTree < getHeight rTree = insert x lTree
            | otherwise                         = lTree
          newRTree
            | getHeight lTree < getHeight rTree = rTree
            | otherwise                         = insert x rTree
          newHeight                             = 1 + max (getHeight newLTree) (getHeight newRTree)

getHeight :: Tree a -> Integer
getHeight Leaf           = 0
getHeight (Node h _ _ _) = h

-- Helper functions to verify the solution for Exercise 2
isBalanced :: Tree a -> Bool
isBalanced Leaf                   = True
isBalanced (Node _ lTree _ rTree) = (diff <= 1) && isBalanced lTree && isBalanced rTree
    where diff = abs (computeHeight lTree - computeHeight rTree)

computeHeight :: Tree a -> Integer
computeHeight Leaf                   = 0
computeHeight (Node _ Leaf _ Leaf)   = 0
computeHeight (Node _ lTree _ rTree) = 1 + max (computeHeight lTree) (computeHeight rTree)


-- Exercise 3
xor :: [Bool] -> Bool
xor = odd
    . foldr (\_ acc -> acc+1) 0
    . filter (==True)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> (f x):acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)


-- Exercise 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = ( map (\x -> 2*x + 1)
                  . ([1..n] \\)
                  . filter (<= n)
                  . map (\(i,j) -> i + j + 2*i*j)
                  . filter (uncurry (<=))) (cartProd [1..n] [1..n])

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
