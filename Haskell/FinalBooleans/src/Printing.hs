{-# LANGUAGE InstanceSigs #-}
module Printing where

import BooleanExpression
import VariableBooleanExpression

newtype Clean = Clean { runClean :: String }

instance BooleanExpression Clean where
  constant :: Bool -> Clean
  constant = Clean . show
  inversion :: Clean -> Clean
  inversion value = Clean $ "not " ++ runClean value
  conjunction :: Clean -> Clean -> Clean
  conjunction left right = Clean $ runClean left ++ " and " ++ runClean right
  disjunction :: Clean -> Clean -> Clean
  disjunction left right = Clean $ runClean left ++ " or " ++ runClean right

instance VariableBooleanExpression Clean where
  variable :: String -> Clean
  variable = Clean

-- Handy when expression is already simplified and prettified
cleanView :: Clean -> String
cleanView = runClean
