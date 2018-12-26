module Normalization where

import Internal.ConjunctionFlip
import Internal.RightNormalization
import Internal.NegationPushing
import Internal.DisjunctionFlattening
import Internal.ConjunctionFlattening

{- Looks somewhat terrifying, huh?
 - Imagine what it would be like
 - if I hadn't introduced all the newtypes out there
 -}
type Prettification c = DisjunctionFlattening (ConjunctionFlattening c)
type Normalization c = NegationPushing (RightNormalization (Flip (RightNormalization (Flip c))))

normalize :: Normalization (Prettification c) -> c
normalize = prettify . flipConjunctions . rightNormalize . flipConjunctions . rightNormalize . pushNegations

prettify :: Prettification c -> c
prettify = flattenConjunctions . flattenDisjunctions
