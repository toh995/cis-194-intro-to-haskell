{-# OPTIONS_GHC -Wall #-}

module AParser where

import Control.Applicative
import Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing -- fail on the empty input
    f (x : xs) -- check if x satisfies the predicate
    -- if so, return x along with the remainder
    -- of the input (that is, xs)
      | p x = Just (x, xs)
      | otherwise = Nothing -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

\*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
\*Parser> runParser (satisfy isUpper) "abc"
Nothing
\*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where
        (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Exercise 1
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap g = (<*>) $ pure g

-- Exercise 2
instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser (\s -> Just (x, s))

  (<*>) :: (Parser (a -> b)) -> (Parser a) -> (Parser b)
  (<*>) (Parser {runParser = p1}) (Parser {runParser = p2}) = Parser g
    where
      g s = case (p1 s) of
        (Nothing) -> Nothing
        (Just (h, s')) ->
          case (p2 s') of
            (Nothing) -> Nothing
            (Just (x, s'')) -> Just (h x, s'')

-- Exercise 3
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\x _ z -> [x, z]) <$> posInt <*> char ' ' <*> posInt

-- Exercise 4
instance Alternative Parser where
  empty :: Parser a
  empty = Parser (const Nothing)

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser {runParser = p1}) (Parser {runParser = p2}) = Parser g
    where
      g s = case (p1 s, p2 s) of
        (Just x, _) -> Just x
        (Nothing, res) -> res

-- Exercise 5
intOrUppercase :: Parser ()
intOrUppercase =
  (const () <$> posInt)
    <|> (const () <$> satisfy isUpper)
