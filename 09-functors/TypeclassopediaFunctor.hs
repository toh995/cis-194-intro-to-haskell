-- Implementing the functor exercises from
-- https://wiki.haskell.org/Typeclassopedia#Functor
{-# OPTIONS_GHC -Wall #-}

module TypeclassopediaFunctors where

---------------
-- INSTANCES --
---------------
-- Exercise 1

-- Haskell will complain because these Functors are already
-- defined in the standard libraries

-- instance Functor (Either e) where
--   fmap g (Left x) = Left x
--   fmap g (Right x) = Right (g x)

-- instance Functor ((->) e) where
--   fmap g h = g . h

-- Exercise 2

-- Already defined in standard library
-- instance Functor ((,) e) where
--   fmap g (x, y) = (x, g y)
data Pair a = Pair a a

instance Functor Pair where
  fmap g (Pair x y) = Pair (g x) (g y)

-- Exercise 3
data ITree a
  = Leaf (Int -> a)
  | Node [ITree a]

instance Functor ITree where
  fmap :: (a -> b) -> ITree a -> ITree b
  fmap g (Leaf f) = Leaf (g . f)
  fmap g (Node xs) = Node $ map (fmap g) xs

-- Exercise 4
-- Answered here: https://stackoverflow.com/questions/16118414/an-example-of-a-type-with-kind-which-cannot-be-an-instance-of-functor

-- Exercise 5
data Compose f g a = Compose (f (g a))

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap :: (a -> b) -> Compose f g a -> Compose f g b
  fmap h (Compose x) = Compose (fmap (fmap h) x)

----------
-- LAWS --
----------
-- Exercise 1
-- Answered here: https://github.com/isaactwong/typeclassopedia/blob/250e272/functors.hs#L67-L78
data Break a = Yes | No deriving (Eq)

instance Functor Break where
  fmap _ _ = No

-- fmap id Yes == Yes -- False
-- fmap (id . (const Yes)) Yes == (fmap id . fmap (const Yes)) Yes -- True
