module Evaluation where

import           DeBruijnLambdasInternal
import           EvaluationData
import           LambdaInfo
import           Test.Tasty
import           Test.Tasty.HUnit

evaluationTests :: TestTree
evaluationTests = testGroup "Evaluation tests"
  [ testIncrementInSimpleLambda
  , testIncrementInBadLambda
  , testSimpleInline
  , testInlineInBadLambda
  , testInlineInCombinator
  , testInlineLambdaWithBadReferences
  , testEvaluateConstants
  , testEvaluateSimpleAddition
  , testConstLambda
  , testTwoAdditions
  ]

-- ---- incrementIndices tests ----

testIncrementInSimpleLambda :: TestTree
testIncrementInSimpleLambda = testCase "Increment indices in simple lambda" $
  assertEqual "" expected result where
    result = incrementIndices simpleAddition
    expected = simpleAddition

testIncrementInBadLambda :: TestTree
testIncrementInBadLambda = testCase "Increment indices in non-combinator lambda" $
  assertEqual "" expected result where
      result = incrementIndices lambdaWithBadReferences
      expected =
        Application
          (Abstraction
            (Operator
              (Variable 0)
              (Variable 2)
            )
          )
          (Constant 42)

-- ---- inline tests ----

testSimpleInline :: TestTree
testSimpleInline = testCase "Inline into simple lambda" $
  assertEqual "" expected result where
    result = inline simpleAdditionBody 0 $ Constant 3
    expected = Operator (Constant 3) (Constant 3)

testInlineInCombinator :: TestTree
testInlineInCombinator = testCase "Inline into combinator" assertion where
  assertion = do
    assertEqual "" combinator $ inline combinator 0 $ Constant 42
    assertEqual "" combinator $ inline combinator 1 $ Constant 42
    assertEqual "" combinator $ inline combinator 2 $ Constant 42
    assertEqual "" combinator $ inline combinator 3 $ Constant 42

testInlineInBadLambda :: TestTree
testInlineInBadLambda = testCase "Inline into non-combinator lambda" $
  assertEqual "" expected result where
    result = inline lambdaWithBadReferences 0 $ Constant 42
    expected =
      Application
        (Abstraction
          (Operator
            (Variable 0)
            (Constant 42)
          )
        )
        (Constant 42)

testInlineLambdaWithBadReferences :: TestTree
testInlineLambdaWithBadReferences = testCase "Inline lambda with external references" $
  assertEqual "" expected result where
    result = inline combinatorBody 0 lambdaWithBadReferences
    expected =
      Abstraction
        (Application
          (Application
            (Abstraction
              (Operator
                (Variable 0)
                (Variable 2)
              )
            )
            (Constant 42)
          )
          (Variable 0)
        )

-- evaluate tests

testEvaluateConstants :: TestTree
testEvaluateConstants = testCase "Evaluate constants" $
  assertEqual "" expected result where
    result :: Maybe (Lambda LambdaInfo)
    result = evaluateByName $ Operator (Constant 2) $ Operator (Constant 4) (Constant 3)
    expected = Just $ Constant 9

testEvaluateSimpleAddition :: TestTree
testEvaluateSimpleAddition = testCase "Simple addition lambda" $
  assertEqual "" expected result where
    result = evaluateByName simpleAddition
    expected = Just $ Constant 6

testConstLambda :: TestTree
testConstLambda = testCase "Constant lambda" $
  assertEqual "" expected result where
    result = evaluateByName $
      Application (Application constLambda $ Constant 8) (Constant 7)
    expected = Just $ Constant 8

testTwoAdditions :: TestTree
testTwoAdditions = testCase "Two additions" $
  assertEqual "" expected result where
    result = evaluateByName doubleAddition
    expected = Just $ Constant 111
