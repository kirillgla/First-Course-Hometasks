module Spec where

import Test.Tasty
import ParserTest
import LexerTest

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests = testGroup "Unit tests" [ lexerTests, parserTests ]
