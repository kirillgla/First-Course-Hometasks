module ParserTest ( parserTests ) where

import Test.Tasty
import Parser.BasicTest
import Parser.OperatorTest
import Parser.LargeScaleTest

parserTests :: TestTree
parserTests = testGroup "Parser tests: "
  [ basicTests
  , operatorTests
  , largeScaleTests
  ]