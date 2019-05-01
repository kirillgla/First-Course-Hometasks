module Semantics where

import Data.Functor

import Formula
import SemanticDomain
import Interpretation
import Operators
import BinaryOperation

semantics :: Ord v => Interpretation v -> Formula v -> Maybe SemanticDomain
semantics interpretation (Variable x) = lookup x (runInterpretation interpretation)
semantics interpretation (Negation f) = semantics interpretation f <&> negation
semantics interpretation (Operation operation left right) = do
  leftSemantics <- semantics interpretation left
  rightSemantics <- semantics interpretation right
  runBinaryOperation operation leftSemantics rightSemantics |> return

