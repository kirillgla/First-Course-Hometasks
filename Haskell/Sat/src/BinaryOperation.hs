{-# LANGUAGE InstanceSigs #-}
module BinaryOperation where

import SemanticDomain

data BinaryOperation =
    Implication
  | Conjunction
  | Disjunction
  deriving (Eq, Ord, Enum, Bounded)

runBinaryOperation :: BinaryOperation -> SemanticDomain -> SemanticDomain -> SemanticDomain
runBinaryOperation Implication = implication
runBinaryOperation Conjunction = conjunction
runBinaryOperation Disjunction = disjunction

instance Show BinaryOperation where
  show :: BinaryOperation -> String
  show Conjunction = "&&"
  show Disjunction = "||"
  show Implication = "->"
