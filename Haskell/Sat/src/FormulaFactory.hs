module FormulaFactory ( formulas, goodFormulas ) where

import Formula
import PropositionalVariable
import BinaryOperation
import Enumerable
import Operators

rawFormulas :: (Int -> [Formula PropositionalVariable]) -> Int -> [Formula PropositionalVariable]
rawFormulas f h
  | h <= 0 = []
  | otherwise =
    [ Variable v | v <- enumerate ] ++
    smaller ++
    [ Negation formula | formula <- smaller ] ++
    [ Operation operation left right | operation <- enumerate, left <- smaller, right <- smaller ]
    where
    smaller :: [Formula PropositionalVariable]
    smaller = f (pred h)

fix :: (a -> a) -> a
fix f = let x = f x in x

memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)

formulas :: Int -> [Formula PropositionalVariable]
formulas = memoize . rawFormulas |> fix

atoms :: [Formula PropositionalVariable]
atoms = [ Variable v | v <- enumerate ] ++ [ Negation (Variable v) | v <- enumerate ]

implications :: [Formula PropositionalVariable]
implications = [ Operation Implication u v | u <- atoms, v <- atoms ]

rawGoodFormulas :: (Int -> [Formula PropositionalVariable]) -> Int -> [Formula PropositionalVariable]
rawGoodFormulas f h
  | h <= 0 = []
  | h == 1 = implications
  | otherwise =
    smaller ++ 
    [ Operation Conjunction left right | left <- smaller, right <- implications ]
    where
    smaller :: [Formula PropositionalVariable]
    smaller = f (pred h)

goodFormulas :: Int -> [Formula PropositionalVariable]
goodFormulas = memoize . rawGoodFormulas |> fix
