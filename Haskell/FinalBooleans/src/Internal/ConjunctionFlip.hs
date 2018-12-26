{-# LANGUAGE InstanceSigs #-}
module Internal.ConjunctionFlip where

{- Reverse of order of all conjunctions -}

import BooleanExpression

newtype Flip e = Flip { runFlip :: e }

-- Again, assume that negation pushing transformation has already been performed
instance BooleanExpression e => BooleanExpression (Flip e) where
  constant :: Bool -> Flip e
  constant = Flip . constant

  inversion :: Flip e -> Flip e
  inversion = Flip . inversion . runFlip

  conjunction :: Flip e -> Flip e -> Flip e
  conjunction left right = Flip $ conjunction (runFlip right) (runFlip left)

  disjunction :: Flip e -> Flip e -> Flip e
  disjunction left right = Flip $ disjunction (runFlip left) (runFlip right)

flipConjunctions :: Flip e -> e
flipConjunctions = runFlip
