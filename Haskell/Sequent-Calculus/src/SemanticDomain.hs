{-# LANGUAGE InstanceSigs #-}
module SemanticDomain where

data SemanticDomain = Zero | One

instance Show SemanticDomain where
  show :: SemanticDomain -> String
  show Zero = "0"
  show One = "1"
