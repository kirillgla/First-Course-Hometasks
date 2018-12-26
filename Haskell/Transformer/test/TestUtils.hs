module TestUtils where

import Test.Tasty
import Test.Tasty.HUnit

testEqual :: (Eq a, Show a) => String -> a -> a -> TestTree
testEqual name expected actual = testCase name $ assertEqual "" actual expected
