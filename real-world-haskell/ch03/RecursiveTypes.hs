-- Exercise 1
data List a = Cons a (List a)
            | Nil
  deriving Show

toList :: List a -> [a]
toList Nil         = []
toList (Cons x xs) = x : toList xs

-- Exercise 2
data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))
  deriving Show
