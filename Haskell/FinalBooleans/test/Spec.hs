module Spec where

import Test.Tasty
import ParserTest

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests = testGroup "Unit tests:" [ parserTests ]
