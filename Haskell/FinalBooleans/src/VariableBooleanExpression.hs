{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module VariableBooleanExpression where

{- Intentionally created
 - separately from main expression
 - to demonstrate extensibility
 -}

import BooleanExpression
import Internal.ConjunctionFlattening
import Internal.ConjunctionFlip
import Internal.DisjunctionFlattening
import Internal.NegationPushing
import Internal.RightNormalization

class VariableBooleanExpression e where
  variable :: String -> e

instance VariableBooleanExpression String where
  variable :: String -> String
  variable = id

instance (BooleanExpression e, VariableBooleanExpression e) => VariableBooleanExpression (ConjunctionFlattening e) where
  variable :: String -> ConjunctionFlattening e
  variable name = ConjunctionFlattening $ \case
    NoConjunction -> variable name
    Conjunction c -> c `conjunction` variable name

instance VariableBooleanExpression e => VariableBooleanExpression (Flip e) where
  variable :: String -> Flip e
  variable = Flip . variable

instance (BooleanExpression e, VariableBooleanExpression e) => VariableBooleanExpression (DisjunctionFlattening e) where
  variable :: String -> DisjunctionFlattening e
  variable name = DisjunctionFlattening $ \case
    NoDisjunction -> variable name
    Disjunction d -> d `disjunction` variable name

instance (BooleanExpression e, VariableBooleanExpression e) => VariableBooleanExpression (NegationPushing e) where
  variable :: String -> NegationPushing e
  variable name = NegationPushing $ \case
    Positive -> variable name
    Negative -> inversion $ variable name

instance (BooleanExpression e, VariableBooleanExpression e) => VariableBooleanExpression (RightNormalization e) where
  variable :: String -> RightNormalization e
  variable name = RightNormalization $ \case
    NoRightConjunction -> variable name
    RightConjunction c -> variable name `conjunction` c
