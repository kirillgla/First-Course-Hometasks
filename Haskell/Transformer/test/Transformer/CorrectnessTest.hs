module Transformer.CorrectnessTest ( correctnessTests ) where

import Test.Tasty

import TransformerData.CorrectnessTest
import TestUtils

correctnessTests :: TestTree
correctnessTests = testGroup "Correctness tests:"
  [ returnTest
  , failTest
  , bindTest
  , complexBindTest
  , irrefutableMatchFailTest
  , alternativeTest
  ]

returnTest :: TestTree
returnTest = testEqual "Return" rawReturn expectedReturn

failTest :: TestTree
failTest = testEqual "Fail" rawFail expectedFail

irrefutableMatchFailTest :: TestTree
irrefutableMatchFailTest = testEqual "Irrefutable pattern match" rawIrrefutableMatchFail expectedIrrefutableMatchFail

bindTest :: TestTree
bindTest = testEqual "Bind" rawBind expectedBind

complexBindTest :: TestTree
complexBindTest = testEqual "Bind with more effects" rawComplexBind expectedComplexBind

alternativeTest :: TestTree
alternativeTest = testEqual "Alternative" rawAlternative expectedAlternative
