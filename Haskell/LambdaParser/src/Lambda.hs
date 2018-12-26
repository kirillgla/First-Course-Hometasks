{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Lambda where

import BinaryOperator

{- 'p' stands for 'parameter type' - what abstraction introduces
 - 'v' stands for 'variable type' - how p is referred to
 -}
data Lambda p v
  = Constant Integer
  | Variable v
  | BinaryOperator BinaryOperator (Lambda p v) (Lambda p v)
  | Abstraction p (Lambda p v)
  | Application (Lambda p v) (Lambda p v)
  | Reset (Lambda p v)
  | Shift p (Lambda p v)
  deriving (Eq, Show)

type NamedLambda = Lambda String String
type DeBruijnLambda = Lambda () Integer

{-
instance Show NamedLambda where
  show (Constant value) = show value
  show (Variable name) = name
  show (BinaryOperator binaryOperator left right) = "(" ++ show left ++ show binaryOperator ++ show right ++ ")"
  show (Abstraction name lambda) = "λ" ++ name ++ "." ++ show lambda
  show (Application body argument) = "(" ++ show body ++ " " ++ show argument ++ ")"
  show (Reset lambda) = "(reset " ++ show lambda ++ ")"
  show (Shift name lambda) = "(shift " ++ name ++ " (" ++ show lambda ++ "))"

instance Show DeBruijnLambda where
  show (Constant value) = show value
  show (Variable n) = "↑" ++ show n
  show (BinaryOperator binaryOperator left right) = "(" ++ show left ++ show binaryOperator ++ show right ++ ")"
  show (Abstraction () lambda) = "λ." ++ show lambda
  show (Application body argument) = "(" ++ show body ++ " " ++ show argument ++ ")"
  show (Reset lambda) = "(reset " ++ show lambda ++ ")"
  show (Shift () lambda) = "(shift " ++ show lambda ++ ")"
-}
