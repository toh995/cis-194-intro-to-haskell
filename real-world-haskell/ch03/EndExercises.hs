import Data.List (sortBy)

-- Exercise 1, 2
listCount :: [a] -> Int
listCount []     = 0
listCount (x:xs) = 1 + listCount xs

-- Exercise 3
computeMean :: [Double] -> Double
computeMean xs = sum xs / fromIntegral (length xs)

-- Exercise 4
makePalindrome :: [a] -> [a]
makePalindrome xs = xs ++ reverse xs

-- Exercise 5
isPalindrome :: [Int] -> Bool
isPalindrome xs = firstHalf == reverse secondHalf
  where len       = length xs
        halfLen   = len `div` 2
        firstHalf = take halfLen xs
        secondHalf
          | even len  = drop halfLen xs
          | otherwise = drop (halfLen + 1) xs

-- Exercise 6
sortByLength :: [[a]] -> [[a]]
sortByLength = sortBy comparator
  where comparator xs ys = compare (length xs) (length ys)

-- Exercise 7
intersperse :: a -> [[a]] -> [a]
intersperse sep []          = []
intersperse sep [x]         = x
intersperse sep (head:tail) = head ++ [sep] ++ intersperse sep tail

-- Exercise 8
data Tree a = Node a (Tree a) (Tree a)
            | Empty
  deriving Show

treeHeight :: Tree a -> Int
treeHeight Empty          = 0
treeHeight (Node _ t1 t2) = 1 + max (treeHeight t1) (treeHeight t2)

-- Exercise 9
data Direction = TurnLeft
               | TurnRight
               | Straight
  deriving Show

-- Exercise 10
data Point = Point Double Double
  deriving Show

data Vector = Vector Double Double
  deriving Show

computeDirection :: Point -> Point -> Point -> Direction
computeDirection a b c
  | angle > 0 = TurnLeft
  | angle < 0 = TurnRight
  | otherwise = Straight
 where v1    = pointsToVector a b
       v2    = pointsToVector b c
       angle = computeAngle v1 v2

pointsToVector :: Point -> Point -> Vector
pointsToVector (Point x1 y1) (Point x2 y2) = Vector (x2-x1) (y2-y1)

computeAngle :: Vector -> Vector -> Double
computeAngle v1 v2 = acos (dotProduct v1 v2 / (magnitude v1 * magnitude v2))

dotProduct :: Vector -> Vector -> Double
dotProduct (Vector x1 y1) (Vector x2 y2) = (x1*y1) + (x2*y2)

magnitude :: Vector -> Double
magnitude (Vector x y) = sqrt (x^2 + y^2)

-- Exercise 11
computeDirections :: [Point] -> [Direction]
computeDirections ps@(p1:p2:p3:_) = computeDirection p1 p2 p3 : computeDirections (tail ps)
computeDirections _ = []
