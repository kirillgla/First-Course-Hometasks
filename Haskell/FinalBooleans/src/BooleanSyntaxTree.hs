{-# LANGUAGE InstanceSigs #-}
module BooleanSyntaxTree where

import BooleanExpression
import VariableBooleanExpression

data BooleanSyntaxTree
  = Constant Bool
  | Inversion BooleanSyntaxTree
  | Conjunction BooleanSyntaxTree BooleanSyntaxTree
  | Disjunction BooleanSyntaxTree BooleanSyntaxTree
  | Variable String
  deriving (Eq, Show)

instance BooleanExpression BooleanSyntaxTree where
  constant :: Bool -> BooleanSyntaxTree
  constant = Constant
  inversion :: BooleanSyntaxTree -> BooleanSyntaxTree
  inversion = Inversion
  conjunction :: BooleanSyntaxTree -> BooleanSyntaxTree -> BooleanSyntaxTree
  conjunction = Conjunction
  disjunction :: BooleanSyntaxTree -> BooleanSyntaxTree -> BooleanSyntaxTree
  disjunction = Disjunction

instance VariableBooleanExpression BooleanSyntaxTree where
  variable :: String -> BooleanSyntaxTree
  variable = Variable

{-
instance Show BooleanSyntaxTree where
  show = finalize
-}

initialize :: BooleanSyntaxTree -> BooleanSyntaxTree
initialize = id

finalize :: (BooleanExpression e, VariableBooleanExpression e) => BooleanSyntaxTree -> e
finalize (Constant c) = constant c
finalize (Inversion i) = inversion $ finalize i
finalize (Conjunction left right) = finalize left `conjunction` finalize right
finalize (Disjunction left right) = finalize left `disjunction` finalize right
finalize (Variable name) = variable name
