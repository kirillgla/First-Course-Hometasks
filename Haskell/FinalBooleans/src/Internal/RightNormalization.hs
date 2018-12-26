{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Internal.RightNormalization where

{- Partial, one-side normalization;
 - Right normalization ensures that
 - expression has no subexpressions of form
 - (a OR b) AND c
 -}

import BooleanExpression

data RightNormalizationContext e = NoRightConjunction | RightConjunction e

newtype RightNormalization e = RightNormalization { runRightNormalization :: RightNormalizationContext e -> e }

-- Assume that negation pushing transformation has already been performed
instance BooleanExpression e => BooleanExpression (RightNormalization e) where
  constant :: Bool -> RightNormalization e
  constant value = RightNormalization $ \case
    NoRightConjunction -> constant value
    RightConjunction c -> constant value `conjunction` c

  inversion :: RightNormalization e -> RightNormalization e
  inversion value = RightNormalization $ \case
    NoRightConjunction -> inversion $ runRightNormalization value NoRightConjunction
    RightConjunction c -> inversion (runRightNormalization value NoRightConjunction) `conjunction` c

  conjunction :: RightNormalization e -> RightNormalization e -> RightNormalization e
  conjunction left right = RightNormalization $ runRightNormalization left . RightConjunction . runRightNormalization right

  disjunction :: RightNormalization e -> RightNormalization e -> RightNormalization e
  disjunction left right = RightNormalization $ \context ->
    runRightNormalization left context `disjunction` runRightNormalization right context

rightNormalize :: RightNormalization e -> e
rightNormalize expression = runRightNormalization expression NoRightConjunction
