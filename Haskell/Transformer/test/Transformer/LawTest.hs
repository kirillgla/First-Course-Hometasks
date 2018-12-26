module Transformer.LawTest ( lawTests ) where

{- These tests are not a proof of correctess, of course -}

import Test.Tasty

import TestUtils
import TransformerData.LawTest

lawTests :: TestTree
lawTests = testGroup "Class laws tests:"
  [ monadLawTests
  , alternativeLawTests
  ]

monadLawTests :: TestTree
monadLawTests = testGroup "Monad laws tests:"
  [ firstMonadLawTest
  , secondMonadLawTest
  , thirdMonadLawTest
  ]

alternativeLawTests :: TestTree
alternativeLawTests = testGroup "Alternative laws tests:"
  [ firstAlternativeLawTest
  , secondAlternativeLawTest
  ]

firstMonadLawTest :: TestTree
firstMonadLawTest = testEqual "First moad law" firstMonadLawLeft firstMonadLawRight

secondMonadLawTest :: TestTree
secondMonadLawTest = testEqual "Second monad law" secondMonadLawLeft secondMonadLawRight

thirdMonadLawTest :: TestTree
thirdMonadLawTest = testEqual "Third monad law" thirdMonadLawLeft thirdMonadLawRight

firstAlternativeLawTest :: TestTree
firstAlternativeLawTest = testEqual "First alternative law" firstAlternativeLawLeft firstAlternativeLawRight

secondAlternativeLawTest :: TestTree
secondAlternativeLawTest = testEqual "Second alternative law" secondAlternativeLawLeft secondAlternativeLawRight
