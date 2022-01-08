{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
import Data.List (scanl')

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]


-- Exercise 2
fibs2 :: [Integer]
fibs2 = fibBuilder [0..]
  where fibBuilder = map fst
                   . scanl
                       (\(fib1, fib2) _ -> (fib2, fib1+fib2))
                       (0,1)


-- Exercise 3
data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs


-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))


-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

ruler :: Stream Integer
ruler = buildRuler 0

buildRuler :: Integer -> Stream Integer
buildRuler n = interleaveStreams (streamRepeat n) (buildRuler (n+1))


-- Exercise 6 
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n                 = Cons n (streamRepeat 0)
  negate                        = streamMap negate
  (Cons a0 a') + (Cons b0 b')   = Cons (a0+b0) (a'+b')
  (Cons a0 a') * b@(Cons b0 b') = Cons (a0*b0) (streamMap (*a0) b' + (a'*b))

instance Fractional (Stream Integer) where
  (Cons a0 a') / (Cons b0 b') = q
    where q = Cons (a0 `div` b0) (streamMap (`div` b0) (a' - q * b'))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)


-- Exercise 7
type Row = (Integer, Integer)
type Col = (Integer, Integer)

newtype Matrix = Matrix ((Integer, Integer), (Integer, Integer))
  deriving (Show)

instance Num Matrix where
  a * b = Matrix ((aRow1 `dotProduct` bCol1, aRow1 `dotProduct` bCol2),
                  (aRow2 `dotProduct` bCol1, aRow2 `dotProduct` bCol2))
    where aRow1 = getRow1 a
          aRow2 = getRow2 a
          bCol1 = getCol1 b
          bCol2 = getCol2 b

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = (snd . getRow1) (startMatrix ^ n)
  where startMatrix = Matrix ((1, 1),
                              (1, 0))

dotProduct :: (Integer, Integer) -> (Integer, Integer) -> Integer
dotProduct (x1, x2) (y1, y2) = x1*y1 + x2*y2

getRow1 :: Matrix -> Row
getRow1 (Matrix (row, _)) = row

getRow2 :: Matrix -> Row
getRow2 (Matrix (_, row)) = row

getCol1 :: Matrix -> Col
getCol1 (Matrix ((a1,_), (a2,_))) = (a1, a2)

getCol2 :: Matrix -> Col
getCol2 (Matrix ((_,a1), (_,a2))) = (a1, a2)
