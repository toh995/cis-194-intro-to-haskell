-- Implementing the functor exercises from
-- https://wiki.haskell.org/Typeclassopedia#Foldable
{-# OPTIONS_GHC -Wall #-}

module TypeclassopediaFoldable where

import Data.Foldable
import Data.Monoid
import qualified Data.Monoid as M
import Data.Semigroup

----------------------------
-- INSTANCES AND EXAMPLES --
----------------------------
-- Exercise 1
class Foldable' t where
  foldMapp' :: Monoid m => (a -> m) -> t a -> m

  fold' :: Monoid m => t m -> m
  fold' = foldMapp' id

-- Exercise 3
class Foldable'' t where
  foldMap'' :: Monoid m => (a -> m) -> t a -> m
  foldMap'' f =
    foldr''
      (\a m -> (f a) <> m)
      mempty

  foldr'' :: (a -> b -> b) -> b -> t a -> b

-- Exercise 4
class Foldable''' t where
  foldMap''' :: Monoid m => (a -> m) -> t a -> m

  foldr''' :: (a -> b -> b) -> b -> t a -> b
  foldr''' f z t =
    let comp = foldMap''' (\a -> Endo (f a)) t
     in appEndo comp z

-------------------
-- DERIVED FOLDS --
-------------------
-- Exercise 1
class Foldable'''' t where
  toList'''' :: t a -> [a]
  toList'''' = foldMap'''' (\a -> [a])

  foldMap'''' :: Monoid m => (a -> m) -> t a -> m

class Foldable''''' t where
  toList''''' :: t a -> [a]
  toList''''' = foldr''''' (\a acc -> a : acc) []

  foldr''''' :: (a -> b -> b) -> b -> t a -> b

-- Exercise 2
class Foldable'''''' t where
  toList'''''' :: t a -> [a]

  foldr'''''' :: (a -> b -> b) -> b -> t a -> b
  foldr'''''' f z t = foldr f z (toList'''''' t)

-- Exercise 3
concat' :: Foldable t => t [a] -> [a]
concat' = fold

concatMap' :: Foldable t => (a -> [b]) -> t a -> [b]
concatMap' = foldMap

and' :: Foldable t => t Bool -> Bool
and' = getAll . foldMap All

or' :: Foldable t => t Bool -> Bool
or' = getAny . foldMap Any

any' :: Foldable t => (a -> Bool) -> t a -> Bool
any' f = getAny . foldMap (Any . f)

all' :: Foldable t => (a -> Bool) -> t a -> Bool
all' f = getAll . foldMap (All . f)

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

maximum' :: (Foldable t, Ord a, Bounded a) => t a -> a
maximum' = getMax . foldMap Max

minimum' :: (Foldable t, Ord a, Bounded a) => t a -> a
minimum' = getMin . foldMap Min

maximumBy' :: Foldable t => (a -> a -> Ordering) -> t a -> a
maximumBy' cmp = foldr1 max'
  where
    max' x y =
      case (cmp x y) of
        (GT) -> x
        _ -> y

minimumBy' :: Foldable t => (a -> a -> Ordering) -> t a -> a
minimumBy' cmp = foldr1 min'
  where
    min' x y =
      case (cmp x y) of
        (LT) -> x
        _ -> y

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a = getAny . foldMap (\b -> Any (a == b))

notElem' :: (Foldable t, Eq a) => a -> t a -> Bool
notElem' a = getAll . foldMap (\b -> All (a /= b))

find' :: Foldable t => (a -> Bool) -> t a -> Maybe a
find' p = M.getFirst . foldMap toMonoid
  where
    toMonoid a
      | p a = M.First (Just a)
      | otherwise = M.First Nothing
