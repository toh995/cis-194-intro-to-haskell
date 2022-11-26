{-# OPTIONS_GHC -Wall #-}
module Scrabble where

-- Exercise 3
newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty = Score 0

-- Point values taken from https://scrabble.hasbro.com/en-us/faq
score :: Char -> Score
score c
  | c `elem` "AaEeIiOoUuLlNnSsTtRr" = Score 1
  | c `elem` "DdGg"                 = Score 2
  | c `elem` "BbCcMmPp"             = Score 3
  | c `elem` "FfHhVvWwYy"           = Score 4
  | c `elem` "Kk"                   = Score 5
  | c `elem` "JjXx"                 = Score 8
  | c `elem` "QqZz"                 = Score 10
  | otherwise                       = Score 0

-- score by scrabble points
scoreString :: String -> Score
-- scoreString = foldr (<>) (Score 0)
--             . map score

-- score by number of words
scoreString s = Score $ (length . words) s
