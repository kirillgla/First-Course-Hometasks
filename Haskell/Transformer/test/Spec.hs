module Spec where

import Test.Tasty

import Transformer.BacktrackableListTTest

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests = testGroup "Unit tests:"
  [ backtrackableListTTests
  ]
