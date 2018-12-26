{-# LANGUAGE FlexibleContexts #-}

module Internal.Lexer where

import Data.Char
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language 
import qualified Text.ParserCombinators.Parsec.Token as Token

isEnglishAlpha :: Char -> Bool
isEnglishAlpha c = c `elem` ['a'..'z'] || c `elem` ['A'..'Z']

isEnglishAlphaNum :: Char -> Bool
isEnglishAlphaNum c = isEnglishAlpha c || isDigit c

englishAlpha :: Stream s m Char => ParsecT s u m Char
englishAlpha = satisfy isEnglishAlpha

englishAlphaNum :: Stream s m Char => ParsecT s u m Char
englishAlphaNum = satisfy isEnglishAlphaNum

lexer :: Token.TokenParser u
lexer = Token.makeTokenParser emptyDef
  { Token.commentStart = ""
  , Token.commentEnd = ""
  , Token.commentLine = ""
  , Token.nestedComments = False
  , Token.identStart = englishAlpha
  , Token.identLetter = englishAlphaNum
  , Token.opStart = oneOf "λ.+-*"
  , Token.opLetter = oneOf ""
  , Token.reservedNames = ["shift", "reset"]
  , Token.reservedOpNames = ["λ", ".", "+", "-", "*"]
  , Token.caseSensitive = True
  }

identifier :: Parser String
identifier = Token.identifier lexer

keyword :: String -> Parser ()
keyword = Token.reserved lexer

number :: Parser Integer
number = Token.integer lexer
--number = Token.decimal lexer

operator :: String -> Parser ()
operator = Token.reservedOp lexer

parenthesis :: Parser a -> Parser a
parenthesis = Token.parens lexer

whitespace :: Parser ()
whitespace = Token.whiteSpace lexer
