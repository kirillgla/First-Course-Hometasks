{-# LANGUAGE ScopedTypeVariables #-}
module TestUtils where

import Test.Tasty
import Test.Tasty.HUnit
import Text.ParserCombinators.Parsec

parserTest :: (Eq a, Show a) => String -> Parser a -> String -> a -> TestTree
parserTest name parser source expected = testCase name $ assertEqual "" (return expected) result where
  result = parse parser "" source

failParserTest :: forall a. Show a => String -> Parser a -> String -> TestTree
failParserTest name parser source = testCase name (either onFailure onSuccess parseResult) where
  parseResult :: Either ParseError a
  parseResult = parse parser "" source
  onSuccess :: a -> Assertion
  onSuccess a = assertBool ("Expected parser to fail, but got result: " ++ show a) False
  onFailure :: b -> Assertion
  onFailure = const $ assertBool "" True
