{-# OPTIONS_GHC -Wall #-}
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
fibs2 = map fst
      . scanl'
          (\(fib1, fib2) _ -> (fib2, fib1+fib2))
          (0,1)
      $ [0 :: Integer ..]

-- Exercise 3
data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show = show . (take 20) . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons y ys) = y : streamToList ys

-- Eyercise 4
streamRepeat :: a -> Stream a
streamRepeat y = Cons y (streamRepeat y)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons y ys) = Cons
                            (f y)
                            (streamMap f ys)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f y = Cons y (streamFromSeed f (f y))

-- Eyercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons y ys) zs = Cons y (interleaveStreams zs ys)

ruler :: Stream Integer
ruler = buildRuler 0

buildRuler :: Integer -> Stream Integer
buildRuler n = interleaveStreams
                 (streamRepeat n)
                 (buildRuler (n+1))

-- Exercise 6
x :: Stream Integer
x = Cons 0 $ Cons 1 (streamRepeat 0)

instance Num (Stream Integer) where
  fromInteger n               = Cons n (streamRepeat 0)
  negate                      = streamMap (negate)
  (+) (Cons y ys) (Cons z zs) = Cons (y+z) (ys + zs)
  (*) (Cons y ys) (Cons z zs) = Cons (y*z)
                                  $ (streamMap (*y) zs)
                                  + (ys * (Cons z zs))

instance Fractional (Stream Integer) where
  (/) (Cons y ys) (Cons z zs) = q
      where q = Cons (y `div` z)
                  $ streamMap (`div` z) (ys - q*zs)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^(2 :: Integer))

-- Exercise 7
data Matrix = Matrix Integer Integer Integer Integer

instance Show Matrix where
  show (Matrix a11 a12 a21 a22) = show [[a11, a12], [a21, a22]]

instance Num Matrix where
  (*) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22)
    = Matrix
        (a11*b11 + a12*b21)
        (a11*b12 + a12*b22)
        (a21*b11 + a22*b21)
        (a21*b12 + a22*b22)
  -- add fromInteger just to suppress an exception...
  fromInteger n = Matrix n n n n


fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = case ((Matrix 1 1 1 0) ^ n) of
              (Matrix _ r _ _) -> r

verifyFib4 :: Bool
verifyFib4 = take 100 fibs2 == map fib4 [0..99]
