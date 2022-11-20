{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Calc where

import ExprT
import StackVM (stackVM, Program, StackVal(..), StackExp(PushI, PushB))
import Parser (parseExp)
import qualified StackVM as StackVM
import qualified Data.Map as M

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit n)     = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr s = case (parsedMaybe) of
                 (Nothing) -> Nothing
                 (Just e)  -> Just (eval e)
    where parsedMaybe = parseExp Lit Add Mul s

-- Exercise 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

-- Exercise 4
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit n
    | n <= 0    = False
    | otherwise = True
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit                       = MinMax
  add (MinMax n) (MinMax m) = MinMax $ max n m
  mul (MinMax n) (MinMax m) = MinMax $ min n m

instance Expr Mod7 where
  lit n                 = Mod7 $ n `mod` 7
  add (Mod7 n) (Mod7 m) = Mod7 $ (n+m) `mod` 7
  mul (Mod7 n) (Mod7 m) = Mod7 $ (n*m) `mod` 7

-- Exercise 5
instance Expr Program where
  lit n = [PushI n]
  add p1 p2 = case (stackVM p1, stackVM p2) of
                   (Left _  , _       ) -> p1
                   (_       , Left _  ) -> p2
                   (Right v1, Right v2) -> valToProgram v1
                                           ++ valToProgram v2
                                           ++ [StackVM.Add]
  mul p1 p2 = case (stackVM p1, stackVM p2) of
                   (Left _  , _       ) -> p1
                   (_       , Left _  ) -> p2
                   (Right v1, Right v2) -> valToProgram v1
                                           ++ valToProgram v2
                                           ++ [StackVM.Mul]

valToProgram :: StackVal -> Program
valToProgram (IVal n) = [PushI n]
valToProgram (BVal b) = [PushB b]
valToProgram (Void)   = []

compile :: String -> Maybe Program
compile = parseExp lit add mul

-- Exercise 6
class HasVars a where
  var :: String -> a

data VarExprT = Lit' Integer
              | Add' VarExprT VarExprT
              | Mul' VarExprT VarExprT
              | Var' String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = Lit'
  add = Add'
  mul = Mul'

instance HasVars VarExprT where
  var = Var'


instance Expr (M.Map String Integer -> Maybe Integer) where
  lit n     = \_ -> Just n
  add f1 f2 = \m -> case (f1 m, f2 m) of
                      (Nothing, _      ) -> Nothing
                      (_      , Nothing) -> Nothing
                      (Just n1, Just n2) -> Just $ n1 + n2
  mul f1 f2 = \m -> case (f1 m, f2 m) of
                      (Nothing, _      ) -> Nothing
                      (_      , Nothing) -> Nothing
                      (Just n1, Just n2) -> Just $ n1 * n2
  
instance HasVars (M.Map String Integer -> Maybe Integer) where
  var s = M.lookup s

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs e = e $ M.fromList vs
