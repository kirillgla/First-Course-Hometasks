{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SemanticConsequence where

import Prelude hiding (foldl)
import Data.Set

import Formula
import Interpretation
import SemanticDomain
import Semantics
import Enumerable

class SemanticConsequence f where
  (|=) :: f -> f -> Bool

equivalent :: SemanticConsequence f => f -> f -> Bool
equivalent f g = (f |= g) && (g |= f)

instance (Ord v, Enum v) => SemanticConsequence (Formula v) where
  (|=) :: Formula v -> Formula v -> Bool
  f |= g = result where
    allVariables :: Set v
    allVariables = union (variables f) (variables g)

    rawInterpretations :: [[(v, SemanticDomain)]]
    rawInterpretations = foldl (\interps variable -> [ (variable, value):interp | interp <- interps, value <- enumerate ]) [[]] allVariables

    interpretations :: [Interpretation v]
    interpretations = fmap Interpretation rawInterpretations

    result :: Bool
    result = all (\interpretation -> if semantics interpretation f == Just One then semantics interpretation g == Just One else True) interpretations
