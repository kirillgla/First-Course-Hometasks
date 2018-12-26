module Parser ( parseLambda ) where

import Text.Parsec
import Internal.Parser
import Lambda

parseLambda :: String -> Either ParseError NamedLambda
parseLambda = parse expression ""
