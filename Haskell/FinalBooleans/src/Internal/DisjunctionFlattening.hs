{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
module Internal.DisjunctionFlattening where

{- Change all disjunctions
 - to left-associative form
 -}

import BooleanExpression

data DisjunctionFlatteningContext e = NoDisjunction | Disjunction e

newtype DisjunctionFlattening e = DisjunctionFlattening { runDisjunctionFlattening :: DisjunctionFlatteningContext e -> e }

instance BooleanExpression e => BooleanExpression (DisjunctionFlattening e) where
  constant :: Bool -> DisjunctionFlattening e
  constant value = DisjunctionFlattening $ \case
    NoDisjunction -> constant value
    Disjunction d -> d `disjunction` constant value

  inversion :: DisjunctionFlattening e -> DisjunctionFlattening e
  inversion value = DisjunctionFlattening $ \case
    NoDisjunction -> inversion $ runDisjunctionFlattening value NoDisjunction
    Disjunction d ->  d `disjunction` inversion (runDisjunctionFlattening value NoDisjunction)

  conjunction :: DisjunctionFlattening e -> DisjunctionFlattening e -> DisjunctionFlattening e
  conjunction left right = DisjunctionFlattening $ \case
    NoDisjunction -> runDisjunctionFlattening left NoDisjunction `conjunction` runDisjunctionFlattening right NoDisjunction
    Disjunction d -> d `disjunction` (runDisjunctionFlattening left NoDisjunction `conjunction` runDisjunctionFlattening right NoDisjunction)

  disjunction :: DisjunctionFlattening e -> DisjunctionFlattening e -> DisjunctionFlattening e
  disjunction left right = DisjunctionFlattening $ runDisjunctionFlattening right . Disjunction . runDisjunctionFlattening left

flattenDisjunctions :: DisjunctionFlattening e -> e
flattenDisjunctions expression = runDisjunctionFlattening expression NoDisjunction
