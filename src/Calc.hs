{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Calc where

import           ExprT
import           Parser
import           StackVM

newtype MinMax =
  MinMax Integer
  deriving (Eq, Show)

newtype Mod7 =
  Mod7 Integer
  deriving (Eq, Show)

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit x = Lit x
  add e1 e2 = ExprT.Add e1 e2
  mul e1 e2 = ExprT.Mul e1 e2

instance Expr Integer where
  lit x = x
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit x = x > 0
  add x y = x || y
  mul x y = x && y

instance Expr MinMax where
  lit x = MinMax x
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
  lit x = Mod7 (mod x 7)
  add (Mod7 x) (Mod7 y) = Mod7 (mod (x + y) 7)
  mul (Mod7 x) (Mod7 y) = Mod7 (mod (x * y) 7)

instance Expr Program where
  lit x = [PushI x]
  add xs ys = (xs ++ ys) ++ [StackVM.Add]
  mul xs ys = (xs ++ ys) ++ [StackVM.Mul]

eval :: ExprT -> Integer
eval (Lit x)           = x
eval (ExprT.Add e1 e2) = (eval e1) + (eval e2)
eval (ExprT.Mul e1 e2) = (eval e1) * (eval e2)

evalStr :: String -> Maybe Integer
evalStr s =
  case (parseExp Lit ExprT.Add ExprT.Mul s) of
    Nothing -> Nothing
    Just e  -> Just (eval e)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3*-4) + 5"

testInteger = testExp :: Maybe Integer

testBool = testExp :: Maybe Bool

testMM = testExp :: Maybe MinMax

testSat = testExp :: Maybe Mod7

testP = testExp :: Maybe Program

compile :: String -> Maybe Program
compile s = (parseExp lit add mul s) :: Maybe Program
