module LexerTest ( lexerTests ) where

import Test.Tasty
import Internal.Lexer
import TestUtils

lexerTests :: TestTree
lexerTests = testGroup "Lexer tests:"
  [ identifierTest
  , integerTest
  , operatorTest
  , parenthesisTest
  , whitespaceTest
  , emptyWhitespaceTest
  ]

identifierTest :: TestTree
identifierTest = parserTest "Identifer parsing" identifier "name" "name"

integerTest :: TestTree
integerTest = parserTest "Integer parsing" number "123" 123

operatorTest :: TestTree
operatorTest = parserTest "Operator parsing" (operator "+") "+" ()

parenthesisTest :: TestTree
parenthesisTest = parserTest "Parenthesis parsing" (parenthesis number) "(123)" 123

whitespaceTest :: TestTree
whitespaceTest = parserTest "Whitespace parsing" whitespace " " ()

emptyWhitespaceTest :: TestTree
emptyWhitespaceTest = parserTest "Empty string as whitespace" whitespace "" ()
