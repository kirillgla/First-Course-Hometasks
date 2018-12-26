module Parser.Lexer where

import Data.Functor
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language 
import qualified Text.ParserCombinators.Parsec.Token as Token

import Parser.Utils

lexer :: Token.TokenParser u
lexer = Token.makeTokenParser emptyDef
  { Token.commentStart = ""
  , Token.commentEnd = ""
  , Token.commentLine = ""
  , Token.nestedComments = False
  , Token.identStart = englishAlpha
  , Token.identLetter = englishAlphaNum
  , Token.opStart = oneOf "ano"
  , Token.opLetter = oneOf "dnort"
  , Token.reservedNames = [ "True", "False", "not", "and", "or" ]
  , Token.reservedOpNames = [ "not", "and", "or" ]
  , Token.caseSensitive = True
  }

identifier :: Parser String
identifier = whitespaces $ Token.identifier lexer

keyword :: String -> Parser ()
keyword = whitespaces . Token.reserved lexer

bool :: Parser Bool
bool = keyword "True" *> pure True <|> keyword "False" *> pure False

number :: Parser Integer
number = whitespaces $ Token.decimal lexer

operator :: String -> Parser ()
operator = whitespaces . Token.reservedOp lexer

parenthesis :: Parser a -> Parser a
parenthesis = whitespaces . Token.parens lexer

-- required for more flexible try-blocks
openingParenthesis :: Parser ()
openingParenthesis = whitespaces $ char '(' $> ()

closingParenthesis :: Parser ()
closingParenthesis = whitespaces $ char ')' $> ()

whitespaces :: Parser a -> Parser a
whitespaces parser = whitespace *> parser <* whitespace where
  whitespace = Token.whiteSpace lexer
