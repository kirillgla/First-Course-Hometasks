module Associativity where

import BinaryOperation

data Associativity = LeftAssociativity | RightAssociativity deriving Eq

associativity :: BinaryOperation -> Associativity
associativity Conjunction = LeftAssociativity
associativity Disjunction = LeftAssociativity
associativity Implication = RightAssociativity

isLeftAssociative :: BinaryOperation -> Bool
isLeftAssociative = (== LeftAssociativity) . associativity