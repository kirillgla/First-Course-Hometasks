module TransformerData.Examples where

import Transformer.BacktrackableListT

simpleValue :: BacktrackableListT Maybe Integer
simpleValue = BacktrackableListT $ map Just [42, 43]

complexValue :: BacktrackableListT Maybe Integer
complexValue = BacktrackableListT [Nothing, Just 42, Nothing]

simplePromoter :: Integer -> BacktrackableListT Maybe Integer
simplePromoter x = BacktrackableListT [Just x, Just 0, Nothing]

simplePromoter2 :: Integer -> BacktrackableListT Maybe Integer
simplePromoter2 x = BacktrackableListT [Just x, Just x]

-- == simpleValue >>= simplePromoter
simpleResult :: BacktrackableListT Maybe Integer
simpleResult = BacktrackableListT [Just 42, Just 0, Nothing, Just 43, Just 0, Nothing]

-- == complexValue >>= simplePromoter
complexResult :: BacktrackableListT Maybe Integer
complexResult = BacktrackableListT $ [Nothing, Just 42, Just 0, Nothing, Nothing]
