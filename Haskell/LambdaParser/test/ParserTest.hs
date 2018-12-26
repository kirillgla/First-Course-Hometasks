module ParserTest ( parserTests ) where

import Test.Tasty

import Parser.BasicTest
import Parser.LargeScaleTest
import Parser.DelimitedContinuationTest

parserTests :: TestTree
parserTests = testGroup "Parser tests:"
  [ basicTests
  , largeScaleTests
  , delimitedContinuationTests
  ]
