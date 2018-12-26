module TestUtils where

import Test.Tasty
import Test.Tasty.HUnit
import Text.ParserCombinators.Parsec
import Data.Either

parserTest :: (Eq a, Show a) => String -> Parser a -> String -> a -> TestTree
parserTest name parser source expected = testCase name $ assertEqual "" (return expected) result where
  result = parse parser "" source

failParserTest :: String -> Parser a -> String -> TestTree
failParserTest name parser source = testCase name $ assertBool "" $ isLeft $ parse parser "" source