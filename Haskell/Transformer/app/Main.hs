module Main where

import Control.Monad

import ListTSymantics
import Transformer.BacktrackableListT
import SyntaxTree

main :: IO ()
main = do
  putStrLn ""
  putStr "Interpreting initial-style example: "
  print . runBacktrackableListT . interpret $ initialExample
  putStr "Interpreting final-style example:   "
  print $ runBacktrackableListT (example :: BacktrackableListT Maybe Integer)
  putStr "Do-example:                         "
  print . runBacktrackableListT . doExample . BacktrackableListT $ [Just (-12), Nothing, Just 42, Just 1]


example :: ListTSymantics s => s Integer
example = (0 `cons` (42 `cons` failure "Oops")) `bind` (\input -> singleton input `choice` singleton (input + 1))

initialExample :: SyntaxTree Maybe Integer
-- initialExample = example
initialExample = (Singleton 0 :|: Singleton 42 :|: Failure "Oops") :>>=: (\input -> Singleton input :|: Singleton (input + 1))

doExample :: BacktrackableListT Maybe Integer -> BacktrackableListT Maybe Integer
doExample source = do
  element <- source
  guard (element > 0)
  if element == 42
  then 100500 `cons` singleton 100500
  else return (element + 1)
