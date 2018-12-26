module Internal.Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Data.Foldable

import Internal.Lexer

import Lambda
import BinaryOperator

constant :: Parser NamedLambda
constant = Constant <$> number <?> "number"

variable :: Parser NamedLambda
variable = Variable <$> identifier <?> "variable name"

abstraction :: Parser NamedLambda
abstraction = do
  operator "λ" <?> "abstraction"
  name <- identifier <?> "parameter name"
  operator  "."
  body <- expression
  return $ Abstraction name body

application :: Parser NamedLambda
application = foldl1 Application <$> many1 (try term)

term :: Parser NamedLambda
term = asum [ parenthesis expression, constant, variable, abstraction, continuation ]

continuation :: Parser NamedLambda
continuation = reset <|> shift <?> "keyword"

reset :: Parser NamedLambda
reset = do
  keyword "reset"
  lambda <- expression
  return $ Reset lambda

shift :: Parser NamedLambda
shift = do
  keyword "shift"
  name <- identifier
  body <- expression
  return $ Shift name body

-- expression is term that can contain operators
expression :: GenParser Char () NamedLambda
expression = buildExpressionParser operators application

operators :: [[Operator Char () NamedLambda]]
operators = [ [ Infix (operator "*" *> return (BinaryOperator Multiplication)) AssocLeft ]
            , [ Infix (operator "+" *> return (BinaryOperator Addition)) AssocLeft
              , Infix (operator "-" *> return (BinaryOperator Subtraction)) AssocLeft
              ]
            ]
