module Operators where

infixl 1 |>
infixr 0 <|

(<|) :: (a -> b) -> a -> b
(<|) = ($)

(|>) :: a -> (a -> b) -> b
(|>) = flip (<|)
