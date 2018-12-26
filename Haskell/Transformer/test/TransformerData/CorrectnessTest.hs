module TransformerData.CorrectnessTest where

import Control.Applicative

import Transformer.BacktrackableListT
import TransformerData.Examples

rawReturn :: BacktrackableListT Maybe Integer
rawReturn = return 42

expectedReturn :: BacktrackableListT Maybe Integer
expectedReturn = BacktrackableListT [Just 42]

rawFail :: BacktrackableListT Maybe Integer
rawFail = fail "Oops"

rawIrrefutableMatchFail :: BacktrackableListT Maybe Integer
rawIrrefutableMatchFail = do
  True <- return False
  return 42

expectedIrrefutableMatchFail :: BacktrackableListT Maybe Integer
expectedIrrefutableMatchFail = BacktrackableListT []

-- not sure about this one
expectedFail :: BacktrackableListT Maybe Integer
expectedFail = BacktrackableListT []

rawBind :: BacktrackableListT Maybe Integer
rawBind = simpleValue >>= simplePromoter

expectedBind :: BacktrackableListT Maybe Integer
expectedBind = simpleResult

rawComplexBind :: BacktrackableListT Maybe Integer
rawComplexBind = complexValue >>= simplePromoter

expectedComplexBind :: BacktrackableListT Maybe Integer
expectedComplexBind = complexResult

rawAlternative :: BacktrackableListT Maybe Integer
rawAlternative = pure 42 <|> pure 43

expectedAlternative :: BacktrackableListT Maybe Integer
expectedAlternative = BacktrackableListT [Just 42, Just 43]

-- todo: test guard
