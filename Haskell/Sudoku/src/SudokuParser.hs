module SudokuParser ( sudokuParser ) where

import Operators
import Board
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Data.Functor

digits :: [Parser Int]
digits = zipWith (($>) . char) ['1'..'9'] [1..9]

hole :: Parser (Maybe a)
hole = char '_' $> Nothing

element :: Parser (Maybe Int)
element = hole:fmap (fmap Just) digits |> choice

line :: Parser [Maybe Int]
line = repeatParse 9 element <* endOfLine

sudokuParser :: Parser (Board (Maybe Int))
sudokuParser = (repeatParse 9 line <* eof) <&> Board

repeatParse :: Int -> Parser a -> Parser [a]
repeatParse n parser
  | n <= 0 = pure []
  | otherwise = do
    parsed <- parser
    parsedRest <- repeatParse (pred n) parser
    parsed:parsedRest |> return