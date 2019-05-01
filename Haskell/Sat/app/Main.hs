module Main where

import Data.Functor

import PropositionalVariable
import BinaryOperation
import Formula
import SemanticConsequence
import FormulaFactory
import Operators

example :: Formula PropositionalVariable
example = Operation Implication (
    Operation Disjunction (Variable A) (Variable B)
  ) (Variable C)

constExample :: Formula PropositionalVariable
constExample = Operation Implication (Variable A) (
    Operation Implication (Variable B) (Variable A)
  )

modusPonens :: Formula PropositionalVariable
modusPonens = Operation Implication (Variable A) (
    Operation Implication (
      Operation Implication (Variable A) (Variable B)
    ) (Variable B)
  )

complicatedExample :: Formula PropositionalVariable
complicatedExample =
  let abc = Operation Implication (Variable A) (Operation Implication (Variable B) (Variable C)) in
  let ac = Operation Implication (Variable A) (Variable C) in
  let ab = Operation Implication (Variable A) (Variable B) in
  let abac = Operation Implication ab ac in
  let abcabac = Operation Implication abc abac in
  abcabac

eitherDestructor :: Formula PropositionalVariable
eitherDestructor =
  let ac = Operation Implication (Variable A) (Variable C) in
  let bc = Operation Implication (Variable B) (Variable C) in
  let e = Operation Disjunction (Variable A) (Variable B) in
  let ec = Operation Implication e (Variable C) in
  let bcec = Operation Implication bc ec in
  let acbcec = Operation Implication ac bcec in
  acbcec

main :: IO ()
main = do
  {- print example
  print constExample
  print modusPonens
  print complicatedExample
  print eitherDestructor
  print (eitherDestructor |= complicatedExample)-}
  let limit = 5
  let template = Operation Disjunction (Operation Disjunction (Variable A) (Variable B)) (Variable C)
  putStr "Finding equivalent for "
  print template
  putStr "Searching for formulae of height <= "
  print limit
  let goods = goodFormulas limit :: [Formula PropositionalVariable]
  let results = goods |> filter (\it -> it `equivalent` template)
  printResults results
  putStr "Number of formulas checked: "
  length goods |> print
  putStr "Total numer of results:     "
  length results |> print

printResults :: [Formula PropositionalVariable] -> IO ()
printResults results = do
  let big = results |> take 11 |> length |> (>= 10)
  if big then putStrLn "Showing first 10 results."
  else return ()
  results |> take 10 <&> print |> sequence_
