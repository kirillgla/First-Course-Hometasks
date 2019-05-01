{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Formula (Formula(..), variables) where

import BinaryOperation
import Associativity
import Data.Set

data Formula v =
    Variable v
  | Negation (Formula v)
  | Operation BinaryOperation (Formula v) (Formula v)

data ChildKind = LeftChild | RightChild

isLeftChild :: ChildKind -> Bool
isLeftChild LeftChild = True
isLeftChild RightChild = False

data ShowContext =
    NoContext
  | NegationContext
  | OperationContext BinaryOperation ChildKind

instance Show v => Show (Formula v) where
  show :: Formula v -> String
  show = contextShow NoContext where
    showOperation :: BinaryOperation -> Formula v -> Formula v -> String
    showOperation operation left right = shower LeftChild left ++ " " ++ show operation ++ " " ++ shower RightChild right where
      shower :: ChildKind -> Formula v -> String
      shower = contextShow . OperationContext operation

    contextShow :: ShowContext -> Formula v -> String
    contextShow _ (Variable v) = show v
    contextShow _ (Negation f) = "NOT " ++ contextShow NegationContext f
    contextShow context (Operation operation left right) = handle context operation content where
      content :: String
      content = showOperation operation left right

      paren :: String -> String
      paren text = "(" ++ text ++ ")"

      handle :: ShowContext -> BinaryOperation -> String -> String
      handle NoContext _ = id
      handle NegationContext _ = paren
      handle (OperationContext outerOperation childKind) innerOperation
        | outerOperation > innerOperation = paren
        | outerOperation < innerOperation = id
        | isLeftAssociative innerOperation == isLeftChild childKind = id
        | otherwise = paren

variables :: Ord v => Formula v -> Set v
variables (Variable v) = singleton v
variables (Negation f) = variables f
variables (Operation _ left right) = union (variables left) (variables right)
