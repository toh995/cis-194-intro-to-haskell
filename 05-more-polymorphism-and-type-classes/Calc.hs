{-# LANGUAGE FlexibleInstances #-}
module Calc where

import qualified Data.Map as M
import Data.Maybe (fromJust, isJust, isNothing)

import ExprT
import Parser
import StackVM

-- Exercise 1
eval :: ExprT.ExprT -> Integer
eval (ExprT.Lit n)     = n
eval (ExprT.Add e1 e2) = eval e1 + eval e2
eval (ExprT.Mul e1 e2) = eval e1 * eval e2


-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr s
  | isNothing expr = Nothing
  | otherwise      = Just (eval (fromJust expr))
 where expr = parseExp
                ExprT.Lit
                ExprT.Add
                ExprT.Mul
                s


-- Exercise 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT.ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul


-- Exercise 4
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit                       = MinMax
  add (MinMax n) (MinMax m) = MinMax $ max n m
  mul (MinMax n) (MinMax m) = MinMax $ max n m

instance Expr Mod7 where
  lit                   = Mod7
  add (Mod7 n) (Mod7 m) = Mod7 $ (n+m) `mod` 7
  mul (Mod7 n) (Mod7 m) = Mod7 $ (n*m) `mod` 7

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)


-- Exercise 5
instance Expr StackVM.Program where
  lit n = [PushI n]

  add p1 p2 =
    case (stackVM p1, stackVM p2) of
      (Right (IVal n1), Right (IVal n2)) -> [StackVM.Add, PushI n1, PushI n2]
      (Right (BVal b1), Right (BVal b2)) -> [StackVM.Or, PushB b1, PushB b2]
      (_,_)                              -> []

  mul p1 p2 =
    case (stackVM p1, stackVM p2) of
      (Right (IVal n1), Right (IVal n2)) -> [StackVM.Mul, PushI n1, PushI n2]
      (Right (BVal b1), Right (BVal b2)) -> [StackVM.And, PushB b1, PushB b2]
      (_,_)                              -> []

compile :: String -> Maybe Program
compile = parseExp lit add mul

-- Exercise 6
class HasVars a where
  var :: String -> a

data VarExprT = Lit' Integer
              | Var' String
              | Add' VarExprT VarExprT
              | Mul' VarExprT VarExprT
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = Lit'
  add = Add'
  mul = Mul'

instance HasVars VarExprT where
  var = Var'

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit n     = \_ -> Just n
  add f1 f2 = \m -> case (f1 m, f2 m) of
                      (Nothing, _)       -> Nothing
                      (_, Nothing)       -> Nothing
                      (Just n1, Just n2) -> Just (n1 + n2)
  mul f1 f2 = \m -> case (f1 m, f2 m) of
                      (Nothing, _)       -> Nothing
                      (_, Nothing)       -> Nothing
                      (Just n1, Just n2) -> Just (n1 * n2)
