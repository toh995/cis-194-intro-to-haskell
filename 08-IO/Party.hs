{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.Tree
import Employee
import Text.Read

-- Exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e : es) (f + empFun e)

instance Semigroup GuestList where
  (<>) (GL es1 f1) (GL es2 f2) = GL (es1 ++ es2) (f1 + f2)

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Exercise 2
treeFold ::
  (a -> [b] -> b) ->
  Tree a ->
  b
treeFold f (Node v ts) = f v $ map (treeFold f) ts

-- Exercise 3
nextLevel ::
  Employee ->
  [(GuestList, GuestList)] ->
  (GuestList, GuestList)
nextLevel boss pairs =
  (glCons boss glNoSubboss, glWithSubboss)
  where
    (glWithSubboss, glNoSubboss) = mconcat pairs

-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun t =
  max gl1 gl2
  where
    (gl1, gl2) = treeFold nextLevel t

-- Exercise 5
fileName :: String
fileName = "company.txt"

main :: IO ()
main = do
  inputStr <- readFile fileName
  putStrLn $ buildOutputStr inputStr

buildOutputStr :: String -> String
buildOutputStr input =
  case (readEither input) of
    (Left _) -> "Could not parse the file \"" ++ fileName ++ "\"!"
    (Right tree) -> treeToString tree

treeToString :: Tree Employee -> String
treeToString t =
  let (GL es fun) = maxFun t
      firstLine = "Total fun: " ++ show fun
      otherLines = map empName es
   in unlines $ [firstLine] ++ otherLines
