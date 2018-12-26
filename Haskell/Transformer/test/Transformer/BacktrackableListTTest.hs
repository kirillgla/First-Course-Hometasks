module Transformer.BacktrackableListTTest ( backtrackableListTTests ) where

import Test.Tasty

import Transformer.LawTest
import Transformer.CorrectnessTest

backtrackableListTTests :: TestTree
backtrackableListTTests = testGroup "BacktrackableListT tests:"
  [ lawTests
  , correctnessTests
  ]
