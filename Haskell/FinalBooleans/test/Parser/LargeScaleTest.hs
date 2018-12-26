module Parser.LargeScaleTest ( largeScaleTests ) where

import Test.Tasty

import Parser.Parser
import TestUtils
import ParserData.LargeScaleTest

largeScaleTests :: TestTree
largeScaleTests = testGroup "Large scale tests:"
  [ implicationTest
  ]

implicationTest :: TestTree
implicationTest = parserTest "Implication" expression rawImplication parsedImplication
