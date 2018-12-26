{-# LANGUAGE FlexibleContexts #-}
module Parser.Utils where

import Data.Char
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec

isEnglishAlpha :: Char -> Bool
isEnglishAlpha c = c `elem` ['a'..'z'] || c `elem` ['A'..'Z']

isEnglishAlphaNum :: Char -> Bool
isEnglishAlphaNum c = isEnglishAlpha c || isDigit c

englishAlpha :: Stream s m Char => ParsecT s u m Char
englishAlpha = satisfy isEnglishAlpha

englishAlphaNum :: Stream s m Char => ParsecT s u m Char
englishAlphaNum = satisfy isEnglishAlphaNum