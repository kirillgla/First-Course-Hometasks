{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Internal.NegationPushing where

import BooleanExpression

data NegationContext = Negative | Positive

newtype NegationPushing e = NegationPushing { runNegationPushing :: NegationContext -> e }

invert :: NegationContext -> NegationContext
invert Negative = Positive
invert Positive = Negative

instance BooleanExpression e => BooleanExpression (NegationPushing e) where
  constant :: Bool -> NegationPushing e
  constant value = NegationPushing $ \case
    Positive -> constant value
    Negative -> inversion $ constant value

  inversion :: NegationPushing e -> NegationPushing e
  inversion value = NegationPushing $ runNegationPushing value . invert

  conjunction :: NegationPushing e -> NegationPushing e -> NegationPushing e
  conjunction left right = NegationPushing $ \case
    Positive -> runNegationPushing left Positive `conjunction` runNegationPushing right Positive
    Negative -> runNegationPushing left Negative `disjunction` runNegationPushing right Negative

  disjunction :: NegationPushing e -> NegationPushing e -> NegationPushing e
  disjunction left right = NegationPushing $ \case
    Positive -> runNegationPushing left Positive `disjunction` runNegationPushing right Positive
    Negative -> runNegationPushing left Negative `conjunction` runNegationPushing right Negative

pushNegations :: NegationPushing e -> e
pushNegations expression = runNegationPushing expression Positive
