module TransformerData.LawTest where

import Control.Applicative

import TransformerData.Examples
import Transformer.BacktrackableListT

firstMonadLawLeft :: BacktrackableListT Maybe Integer
firstMonadLawLeft = return 42 >>= simplePromoter

firstMonadLawRight :: BacktrackableListT Maybe Integer
firstMonadLawRight = simplePromoter 42

secondMonadLawLeft :: BacktrackableListT Maybe Integer
secondMonadLawLeft = simpleValue >>= return

secondMonadLawRight :: BacktrackableListT Maybe Integer
secondMonadLawRight = simpleValue

thirdMonadLawLeft :: BacktrackableListT Maybe Integer
thirdMonadLawLeft = simpleValue >>= simplePromoter >>= simplePromoter2

thirdMonadLawRight :: BacktrackableListT Maybe Integer
thirdMonadLawRight = simpleValue >>= (\x -> simplePromoter x >>= simplePromoter2)

firstAlternativeLawLeft :: BacktrackableListT Maybe Integer
firstAlternativeLawLeft = empty <|> (BacktrackableListT [Just 42, Nothing, Just 43])

firstAlternativeLawRight :: BacktrackableListT Maybe Integer
firstAlternativeLawRight = BacktrackableListT [Just 42, Nothing, Just 43]

secondAlternativeLawLeft :: BacktrackableListT Maybe Integer
secondAlternativeLawLeft = (BacktrackableListT [Just 42, Nothing]) <|> (pure 0 <|> pure 1)

secondAlternativeLawRight :: BacktrackableListT Maybe Integer
secondAlternativeLawRight = ((BacktrackableListT [Just 42, Nothing]) <|> pure 0) <|> pure 1
