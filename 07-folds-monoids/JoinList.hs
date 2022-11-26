{-# OPTIONS_GHC -Wall #-}
module JoinList where

import qualified Data.Foldable as F

import Buffer
import Editor
import Scrabble
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)


-- Exercise 1
tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x <> tag y) x y

-- Exercise 2
listSize :: (Sized b, Monoid b) =>
            JoinList b a -> Int
listSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ i _
  | i < 0      = Nothing
indexJ _ Empty = Nothing
indexJ i (Single _ x)
  | i == 0     = Just x
  | otherwise  = Nothing
indexJ i l@(Append _ x y)
  | i >= lSize = Nothing
  | i <  xSize = indexJ i x
  | otherwise  = indexJ (i - xSize) y
    where xSize = listSize x
          lSize = listSize l

dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
dropJ n l
  | n <= 0             = l
dropJ _ Empty          = Empty
dropJ _ (Single _ _)   = Empty
dropJ n (Append _ x y) = dropJ n x
                       +++ dropJ (n - listSize x) y

takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
takeJ n _
  | n <= 0             = Empty
takeJ _ Empty          = Empty
takeJ _ l@(Single _ _) = l
takeJ n (Append _ x y) = takeJ n x
                       +++ takeJ (n - listSize x) y

-- Exercise 3
scoreLine :: String -> JoinList Score String
scoreLine s  = Single (scoreString s) s

-- Exercise 4
lineToList :: String -> JoinList (Score, Size) String
lineToList s = Single (scoreString s, 1) s

instance Buffer (JoinList (Score, Size) String) where
  toString Empty          = ""
  toString (Single _ s)   = s
  toString (Append _ x y) = toString x ++ "\n" ++ toString y

  fromString = F.foldr' (+++) Empty
             . map lineToList
             . lines

  line = indexJ

  replaceLine n s l
    | n >= listSize l = l
    | otherwise       = takeJ n l
                      +++ fromString s
                      +++ dropJ (n+1) l

  numLines = listSize

  value l = s
    where (Score s, _) = tag l

main :: IO ()
main = runEditor editor
         $ (fromString :: String -> JoinList (Score, Size) String)
           (unlines
             [ "This buffer is for notes you don't want to save, and for"
             , "evaluation of steam valve coefficients."
             , "To load a different file, type the character L followed"
             , "by the name of the file."
             ])
