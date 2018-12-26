module Parser.LargeScaleTest ( largeScaleTests ) where

import Test.Tasty
import Internal.Parser
import ParserData.LargeScaleTest
import TestUtils

largeScaleTests :: TestTree
largeScaleTests = testGroup "Large scale tests:" [ lazinessDemoTest ]

lazinessDemoTest :: TestTree
lazinessDemoTest = parserTest "Classic laziness demo" expression rawLazinessDemo parsedLazinessDemo
