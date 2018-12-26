{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
module Internal.ConjunctionFlattening where

{- Change all conjunctions
 - to left-associative form
 -}

import BooleanExpression

data ConjunctionFlatteningContext e = NoConjunction | Conjunction e

newtype ConjunctionFlattening e = ConjunctionFlattening { runConjunctionFlattening :: ConjunctionFlatteningContext e -> e }

instance BooleanExpression e => BooleanExpression (ConjunctionFlattening e) where
  constant :: Bool -> ConjunctionFlattening e
  constant value = ConjunctionFlattening $ \case
    NoConjunction -> constant value
    Conjunction c -> c `conjunction` constant value

  inversion :: ConjunctionFlattening e -> ConjunctionFlattening e
  inversion value = ConjunctionFlattening $ \case
    NoConjunction -> inversion $ runConjunctionFlattening value NoConjunction
    Conjunction c ->  c `conjunction` inversion (runConjunctionFlattening value NoConjunction)

  conjunction :: ConjunctionFlattening e -> ConjunctionFlattening e -> ConjunctionFlattening e
  conjunction left right = ConjunctionFlattening $ runConjunctionFlattening right . Conjunction . runConjunctionFlattening left

  disjunction :: ConjunctionFlattening e -> ConjunctionFlattening e -> ConjunctionFlattening e
  disjunction left right = ConjunctionFlattening $ \case
    NoConjunction -> runConjunctionFlattening left NoConjunction `disjunction` runConjunctionFlattening right NoConjunction
    Conjunction c -> c `conjunction` (runConjunctionFlattening left NoConjunction `disjunction` runConjunctionFlattening right NoConjunction)

flattenConjunctions :: ConjunctionFlattening e -> e
flattenConjunctions expression = runConjunctionFlattening expression NoConjunction
