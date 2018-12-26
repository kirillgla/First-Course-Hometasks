module Parser.DelimitedContinuationTest ( delimitedContinuationTests ) where

import Test.Tasty
import Internal.Parser
import ParserData.DelimitedContinuationTest
import TestUtils

delimitedContinuationTests :: TestTree
delimitedContinuationTests = testGroup "Delimited continuation tests:"
  [ resetTest
  , resetUnderParenthesisTest
  , consequtiveResetTest
  , shiftTest
  , idTest
  , just42Test
  , bigContinuationTest
  , smallContinuationTest
  , applicationResetTest
  ]

resetTest :: TestTree
resetTest = parserTest "Reset test" reset rawReset parsedReset

resetUnderParenthesisTest :: TestTree
resetUnderParenthesisTest = parserTest "Sequence of resets with parenthesis" reset rawResetUnderParenthesis parsedResetUnderParenthesis

consequtiveResetTest :: TestTree
consequtiveResetTest = parserTest "Sequence of resets" reset rawConsequtiveReset parsedConsequtiveReset

shiftTest :: TestTree
shiftTest = parserTest "Simple shift" shift rawShift parsedShift

idTest :: TestTree
idTest = parserTest "Id lambda parsing" expression rawId parsedId

just42Test :: TestTree
just42Test = parserTest "42 with shift/reset" expression rawJust42 parsedJust42

bigContinuationTest :: TestTree
bigContinuationTest = parserTest "Big continuation" expression rawBigContinuation parsedBigContinuation

smallContinuationTest :: TestTree
smallContinuationTest = parserTest "Small continuation" expression rawSmallContinuation parsedSmallContinuation

applicationResetTest :: TestTree
applicationResetTest = parserTest "Application under reset" expression rawApplicationReset parsedApplicationReset
