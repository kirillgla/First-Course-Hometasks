module Parser.OperatorTest ( operatorTests ) where

import Text.ParserCombinators.Parsec
import Test.Tasty

import BooleanSyntaxTree
import Parser.Parser
import ParserData.OperatorTest
import TestUtils

operatorTests :: TestTree
operatorTests = testGroup "Operator tests:"
  [ inversionTest
  , conjunctionTest
  , disjunctionTest
  , malformedConjunctionTest
  ]

inversionTest :: TestTree
inversionTest = parserTest "Inversion" expression rawInversion parsedInversion

conjunctionTest :: TestTree
conjunctionTest = parserTest "Conjunction" expression rawConjunction parsedConjunction

disjunctionTest :: TestTree
disjunctionTest = parserTest "Disjunction" expression rawDisjunction parsedDisjunction

malformedConjunctionTest :: TestTree
malformedConjunctionTest = failParserTest "Malformed conjunction" (expression :: Parser BooleanSyntaxTree) rawMalformedConjunction
