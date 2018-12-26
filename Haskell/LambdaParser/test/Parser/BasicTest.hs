module Parser.BasicTest ( basicTests ) where

import Test.Tasty
import Internal.Parser
import ParserData.BasicTest
import TestUtils

basicTests :: TestTree
basicTests = testGroup "Basic tests:"
  [ constantTest
  , variableTest
  , keywordVariableTest
  , abstractionTest
  , constTest
  , applicationTest
  , longApplicationTest
  , longParenthesisApplicationTest
  , sumTest
  , multiplicationTest
  , simpleArithmeticTest
  , arithmeticTest
  ]

constantTest :: TestTree
constantTest = parserTest "Constant parsing" constant rawConstant parsedConstant

variableTest :: TestTree
variableTest = parserTest "Variable parsing" variable rawVariable parsedVariable

keywordVariableTest :: TestTree
keywordVariableTest = parserTest "Keyword containing variable parsing" expression rawKeywordVariable parsedKeywordVariable

abstractionTest :: TestTree
abstractionTest = parserTest "Abstraction parsing" abstraction rawAbstraction parsedAbstraction

constTest :: TestTree
constTest = parserTest "Const lambda parsing" abstraction rawConst parsedConst

applicationTest :: TestTree
applicationTest = parserTest "Application parsing" application rawApplication parsedApplication

longApplicationTest :: TestTree
longApplicationTest = parserTest "Long application parsing" application rawLongApplication parsedLongApplication

longParenthesisApplicationTest :: TestTree
longParenthesisApplicationTest = parserTest "Parsing application with parenthesis" application rawParenthesisApplication parsedParenthesisApplication

sumTest :: TestTree
sumTest = parserTest "Sum" expression rawSum parsedSum

multiplicationTest :: TestTree
multiplicationTest = parserTest "Multiplication" expression rawMultiplication parsedMultiplication

simpleArithmeticTest :: TestTree
simpleArithmeticTest = parserTest "Simple arithmetic expression" expression rawSimpleArithmetic parsedSimpleArithmetic

arithmeticTest :: TestTree
arithmeticTest = parserTest "Arithmetic expression" expression rawArithmetic parsedArithmetic
