{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UnicodeSyntax #-}
module Main where

import BooleanExpression
import VariableBooleanExpression
import Normalization
import Printing

main :: IO ()
main = do
  printEvaluation expression
  putStrLn ""
  printSimplification variableExpression
  putStrLn ""
  printSimplification longExpression

expression :: BooleanExpression b => b
expression = inversion $ inversion (constant True) `disjunction` constant False

variableExpression :: (BooleanExpression b, VariableBooleanExpression b) => b
variableExpression = (variable "x" `disjunction` expression) `conjunction` variable "y"

longExpression :: (BooleanExpression e, VariableBooleanExpression e) => e
longExpression = (variableExpression `conjunction` ((variable "z" `disjunction` variable "a") `conjunction` constant True))
  `disjunction` (variable "b" `conjunction` variable "c")

printEvaluation :: (forall b. BooleanExpression b => b) -> IO ()
printEvaluation e = do
  putStr "Expression: "
  putStr $ view e
  putStr " = "
  print $ evaluate e
  let normalized = normalize e
  putStr "Normalized: "
  putStr $ view normalized
  putStr " = "
  print $ evaluate normalized

printSimplification :: (forall e. (BooleanExpression e, VariableBooleanExpression e) => e) -> IO ()
printSimplification e = do
  putStrLn $ view e
  {- Here it is safe to use this method,
   - provided expression is already normalized;
   - But in case you doubt, here's a better way:
   -}
  -- putStr "  => "
  -- putStrLn $ view $ normalize e
  putStr "  => "
  putStrLn $ cleanView $ normalize e
