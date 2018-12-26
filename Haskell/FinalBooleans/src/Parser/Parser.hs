module Parser.Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

import Parser.Lexer
import qualified BooleanExpression as E
import qualified VariableBooleanExpression as V

constant :: E.BooleanExpression e => Parser e
constant = E.constant <$> bool

variable :: V.VariableBooleanExpression e => Parser e
variable = V.variable <$> identifier

atom :: (E.BooleanExpression e, V.VariableBooleanExpression e) => Parser e
atom = variable <|> constant

expression :: (E.BooleanExpression e, V.VariableBooleanExpression e) => GenParser Char () e
expression = buildExpressionParser operators atom

operators :: E.BooleanExpression e => [[Operator Char () e]]
operators = [ [Prefix (operator "not" *> return E.inversion) ]
            , [ Infix (operator "and" *> return E.conjunction) AssocLeft ]
            , [ Infix (operator "or"  *> return E.disjunction) AssocLeft ]
            ]
