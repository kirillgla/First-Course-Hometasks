{-# LANGUAGE InstanceSigs #-}
module SemanticDomain where

data SemanticDomain = Zero | One deriving (Eq, Ord, Enum, Bounded)

instance Show SemanticDomain where
  show :: SemanticDomain -> String
  show Zero = "0"
  show One = "1"

negation :: SemanticDomain -> SemanticDomain
negation Zero = One
negation One = Zero

conjunction :: SemanticDomain -> SemanticDomain -> SemanticDomain
conjunction One One = One
conjunction _ _ = Zero

disjunction :: SemanticDomain -> SemanticDomain -> SemanticDomain
disjunction Zero Zero = Zero
disjunction _ _ = One

implication :: SemanticDomain -> SemanticDomain -> SemanticDomain
implication a b = disjunction (negation a) b
