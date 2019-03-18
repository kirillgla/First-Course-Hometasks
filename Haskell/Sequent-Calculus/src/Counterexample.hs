{-# LANGUAGE InstanceSigs #-}
module Counterexample where

import Formula
import SemanticDomain

newtype Counterexample = Counterexample { runCounterexample :: [(Variable, SemanticDomain)] }

instance Show Counterexample where
  show :: Counterexample -> String
  show counterexample = "Counterexample:\n"
    ++ foldr (\(variable, value) acc -> show variable ++ " = " ++ show value ++ "\n" ++ acc) "" (runCounterexample counterexample)