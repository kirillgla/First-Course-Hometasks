{-# LANGUAGE InstanceSigs #-}
module Formula where

import Operators
import Data.List

data Formula =
    VariableFormula Variable
  | Conjunction Formula Formula
  | Disjunction Formula Formula
  | Implication Formula Formula
  | Negation Formula

newtype Variable = Variable { runVariable :: String } deriving Eq

data Formulae = Formulae
  { getVariables :: [Variable]
  , getFormulae :: [Formula]
  }

instance Show Variable where
  show :: Variable -> String
  show = runVariable

instance Show Formula where
  show :: Formula -> String
  show (VariableFormula v) = show v
  show (Conjunction n m) = "(" ++ show n ++ " and " ++ show m ++ ")"
  show (Disjunction n m) = "(" ++ show n ++ " or " ++ show m ++ ")"
  show (Implication n m) = "(" ++ show n ++ " -> " ++ show m ++ ")"
  show (Negation n) = "(not" ++ show n ++ ")"

instance Show Formulae where
  show :: Formulae -> String
  show formulae = fmap VariableFormula (getVariables formulae) ++ getFormulae formulae |> fmap show |> intercalate ","

var :: String -> Formula
var = VariableFormula . Variable
