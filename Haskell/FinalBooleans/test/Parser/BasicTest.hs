module Parser.BasicTest ( basicTests ) where

import Test.Tasty
import TestUtils
import Text.ParserCombinators.Parsec

import BooleanSyntaxTree

import Parser.Parser
import ParserData.BasicTest

basicTests :: TestTree
basicTests = testGroup "Basic tests:"
  [ constantTest
  , variableAsConstantTest
  , variableTest
  , constantAsVariableTest
  , constantAsAtomTest
  , variableAsAtomTest
  , constantAsExpressionTest
  , variableAsExpressionTest
  ]

constantTest :: TestTree
constantTest = parserTest "Parsing constant as constant" constant rawConstant parsedConstant

variableAsConstantTest :: TestTree
variableAsConstantTest = failParserTest "Parsing variable as constant" (constant :: Parser Bool) rawVariable

variableTest :: TestTree
variableTest = parserTest "Parsing variable as variable" variable rawVariable parsedVariable

constantAsVariableTest :: TestTree
constantAsVariableTest = failParserTest "Parsing constant as variable" (variable :: Parser BooleanSyntaxTree) rawConstant

constantAsAtomTest :: TestTree
constantAsAtomTest = parserTest "Parsing constant as atom" atom rawConstant parsedConstant

variableAsAtomTest :: TestTree
variableAsAtomTest = parserTest "Parsing variable as atom" atom rawVariable parsedVariable

constantAsExpressionTest :: TestTree
constantAsExpressionTest = parserTest "Parsing constant as expression" expression rawConstant parsedConstant

variableAsExpressionTest :: TestTree
variableAsExpressionTest = parserTest "Parsing variable as expression" expression rawVariable parsedVariable

