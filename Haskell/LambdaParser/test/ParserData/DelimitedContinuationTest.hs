module ParserData.DelimitedContinuationTest where

import Lambda
import BinaryOperator

rawReset :: String
rawReset = "reset 42"

parsedReset :: NamedLambda
parsedReset = Reset $ Constant 42

rawResetUnderParenthesis :: String
rawResetUnderParenthesis = "reset (reset reset1)"

parsedResetUnderParenthesis :: NamedLambda
parsedResetUnderParenthesis = Reset $ Reset $ Variable "reset1"

rawConsequtiveReset :: String
rawConsequtiveReset = "reset reset reset1"

parsedConsequtiveReset :: NamedLambda
parsedConsequtiveReset = Reset $ Reset $ Variable "reset1"

rawShift :: String
rawShift = "shift k k"

parsedShift :: NamedLambda
parsedShift = Shift "k" $ Variable "k"

rawId :: String
rawId = "(reset (shift k k))"

parsedId :: NamedLambda
parsedId = Reset $ Shift "k" $ Variable "k"

rawJust42 :: String
rawJust42 = "(reset (shift k 42))"

parsedJust42 :: NamedLambda
parsedJust42 = Reset $ Shift "k" $ Constant 42

rawBigContinuation :: String
rawBigContinuation = "2*(reset (1+(shift k (k 20))))"

parsedBigContinuation :: NamedLambda
parsedBigContinuation =
  (BinaryOperator Multiplication
    (Constant 2)
    (Reset
      (BinaryOperator Addition
        (Constant 1)
        (Shift "k"
          (Application
            (Variable "k")
            (Constant 20)
          )
        )
      )
    )
  )

rawSmallContinuation :: String
rawSmallContinuation = "(reset (2*(shift k 21)))"

parsedSmallContinuation :: NamedLambda
parsedSmallContinuation =
  (Reset
    (BinaryOperator Multiplication
      (Constant 2)
      (Shift "k"
        (Constant 21)
      )
    )
  )

rawApplicationReset :: String
rawApplicationReset = "reset f g"

parsedApplicationReset :: NamedLambda
parsedApplicationReset =
  (Reset
    (Application
      (Variable "f")
      (Variable "g")
    )
  )
