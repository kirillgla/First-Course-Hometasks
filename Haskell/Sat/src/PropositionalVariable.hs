{-# LANGUAGE InstanceSigs #-}
module PropositionalVariable where

data PropositionalVariable = A | B | C
  deriving (Eq, Ord, Enum, Bounded)

instance Show PropositionalVariable where
  show :: PropositionalVariable -> String
  show A = "a"
  show B = "b"
  show C = "c"
