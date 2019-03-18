module Main where

import Sequention
import Formula
import Operators

constFormula :: Formula
constFormula = var "x" `Implication` (var "y" `Implication` (var "z" `Implication` var "x"))

composition :: Formula
composition = (var "b" `Implication` var "c") `Implication` ((var "a" `Implication` var "b") `Implication` ((var "a") `Implication` (var "c")))

nonTautology :: Formula
nonTautology = var "x" `Implication` (var "y" `Implication` var "z")

toSequention :: Formula -> Sequention
toSequention f = Sequention (Formulae [] []) (Formulae [] [f])

main :: IO ()
main = do
  putStrLn "Tautological example:"
  print composition
  putStrLn ""
  composition |> toSequention |> deriveSequention |> either show show |> putStrLn
  putStrLn ""
  putStrLn "Non-tautological example:"
  print nonTautology
  putStrLn ""
  nonTautology |> toSequention |> deriveSequention |> either show show |> putStrLn
