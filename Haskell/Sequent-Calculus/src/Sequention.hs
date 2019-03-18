{-# LANGUAGE InstanceSigs #-}
module Sequention where

import Formula
import SemanticDomain
import Operators
import Counterexample
import Data.Functor

{- Antecedent |- Succedent -}
{- True       |- False     -}
data Sequention = Sequention
  { getAntecedent :: Formulae
  , getSuccedent :: Formulae
  }

instance Show Sequention where
  show :: Sequention -> String
  show sequention = antecedent ++ antecedentSeparator ++ "|-" ++ succedentSeparator ++ succedent where
    antecedent = show (getAntecedent sequention)
    succedent = show (getSuccedent sequention)
    antecedentSeparator = if null antecedent then "" else " "
    succedentSeparator = if null succedent then "" else " "

data SequentionDerivation =
    AntecedentNegation Sequention SequentionDerivation
  | AntecedentDisjunction Sequention SequentionDerivation SequentionDerivation
  | AntecedentConjunction Sequention SequentionDerivation
  | AntecedentImplication Sequention SequentionDerivation SequentionDerivation

  | SuccedentNegation Sequention SequentionDerivation
  | SuccedentDisjunction Sequention SequentionDerivation
  | SuccedentConjunction Sequention SequentionDerivation SequentionDerivation
  | SuccedentImplication Sequention SequentionDerivation

  | Axiom Sequention

instance Show SequentionDerivation where
  show :: SequentionDerivation -> String
  show = showDerivation 0 where
    step = 2
    showDerivation :: Int -> SequentionDerivation -> String
    showDerivation offset (AntecedentNegation sequention  derivation) =
      showDerivation (offset + step) derivation ++ "\n"
      ++ replicate offset ' ' ++ show sequention ++ " [Negation introduction in antecedent]"
    showDerivation offset (AntecedentDisjunction sequention derivation1 derivation2) =
      showDerivation (offset + step) derivation1 ++ "\n"
      ++ showDerivation (offset + step) derivation2 ++ "\n"
      ++ replicate offset ' ' ++ show sequention ++ " [Disjunction introduction in antecedent]"
    showDerivation offset (AntecedentConjunction sequention derivation) =
      showDerivation (offset + step) derivation ++ "\n"
      ++ replicate offset ' ' ++ show sequention ++ " [Conjunction introduction in antecedent]"
    showDerivation offset (AntecedentImplication sequention derivation1 derivation2) =
      showDerivation (offset + step) derivation1 ++ "\n"
      ++ showDerivation (offset + step) derivation2 ++ "\n"
      ++ replicate offset ' ' ++ show sequention ++ " [Implication introduction in antecedent]"

    showDerivation offset (SuccedentNegation sequention derivation) =
      showDerivation (offset + step) derivation ++ "\n"
      ++ replicate offset ' ' ++ show sequention ++ " [Negation introduction in succedent]"
    showDerivation offset (SuccedentDisjunction sequention derivation) =
      showDerivation (offset + step) derivation ++ "\n"
      ++ replicate offset ' ' ++ show sequention ++ " [Disjunction introduction in succedent]"
    showDerivation offset (SuccedentConjunction sequention derivation1 derivation2) =
      showDerivation (offset + step) derivation1 ++ "\n"
      ++ showDerivation (offset + step) derivation2 ++ "\n"
      ++ replicate offset ' ' ++ show sequention ++ " [Conjunction introduction in succedent]"
    showDerivation offset (SuccedentImplication sequention derivation) =
      showDerivation (offset + step) derivation ++ "\n"
      ++ replicate offset ' ' ++ show sequention ++ " [Implication introduction in succedent]"

    showDerivation offset (Axiom sequention) = replicate offset ' ' ++ show sequention ++ " [Axiom]"

deriveSequention :: Sequention -> Either Counterexample SequentionDerivation
deriveSequention (Sequention (Formulae variables ((VariableFormula v):formulae)) succedent) =
  Formulae (v:variables) formulae |> flip Sequention succedent |> deriveSequention
deriveSequention sequention@(Sequention (Formulae variables ((Conjunction n m):formulae)) succedent) =
  Formulae variables (n:m:formulae) |> flip Sequention succedent |> deriveSequention <&> AntecedentConjunction sequention
deriveSequention sequention@(Sequention (Formulae variables (Disjunction n m:formulae)) succedent) = do
  derivation1 <- Formulae variables (n:formulae) |> flip Sequention succedent |> deriveSequention
  derivation2 <- Formulae variables (m:formulae) |> flip Sequention succedent |> deriveSequention
  AntecedentDisjunction sequention derivation1 derivation2 |> return
deriveSequention sequention@(Sequention (Formulae variables (Implication n m:formulae)) succedent) = do
  derivation1 <- Sequention (Formulae variables formulae) (Formulae (getVariables succedent) (n:getFormulae succedent)) |> deriveSequention
  derivation2 <- Formulae variables (m:formulae) |> flip Sequention succedent |> deriveSequention
  AntecedentImplication sequention derivation1 derivation2 |> return
deriveSequention sequention@(Sequention (Formulae variables (Negation n:formulae)) succedent) =
  Sequention newAntecedent newSuccedent |> deriveSequention <&> AntecedentNegation sequention where
    newAntecedent = Formulae variables formulae
    newSuccedent = Formulae (getVariables succedent) (n:getFormulae succedent)
deriveSequention (Sequention antecedent (Formulae variables ((VariableFormula v):formulae))) =
  Formulae (v:variables) formulae |> Sequention antecedent |> deriveSequention
deriveSequention sequention@(Sequention antecedent (Formulae variables (Conjunction n m:formulae))) = do
  derivation1 <- Formulae variables (n:formulae) |> Sequention antecedent |> deriveSequention
  derivation2 <- Formulae variables (m:formulae) |> Sequention antecedent |> deriveSequention
  SuccedentConjunction sequention derivation1 derivation2 |> return
deriveSequention sequention@(Sequention antecedent (Formulae variables (Disjunction n m:formulae))) =
  Formulae variables (n:m:formulae) |> Sequention antecedent |> deriveSequention <&> SuccedentDisjunction sequention
deriveSequention sequention@(Sequention antecedent (Formulae variables (Implication n m:formulae))) =
  Sequention newAntecedent newSuccedent |> deriveSequention <&> SuccedentImplication sequention where
    newAntecedent = Formulae (getVariables antecedent) (n:getFormulae antecedent)
    newSuccedent = Formulae variables (m:formulae)
deriveSequention sequention@(Sequention antecedent (Formulae variables (Negation n:formulae))) =
  Sequention newAntecedent newSuccedent |> deriveSequention <&> SuccedentNegation sequention where
    newAntecedent = Formulae (getVariables antecedent) (n:getFormulae antecedent)
    newSuccedent = Formulae variables formulae
deriveSequention sequention@(Sequention (Formulae antecedentVariables []) (Formulae succedentVariables []))
  | any (flip elem antecedentVariables) succedentVariables = Axiom sequention |> return
  | otherwise = fmap (flip (,) One) antecedentVariables ++ fmap (flip (,) Zero) succedentVariables |> Counterexample |> Left
