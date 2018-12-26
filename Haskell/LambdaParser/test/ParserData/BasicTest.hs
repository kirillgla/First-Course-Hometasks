module ParserData.BasicTest where

import Lambda
import BinaryOperator

rawConstant :: String
rawConstant = "123"

parsedConstant :: NamedLambda
parsedConstant = Constant 123

rawVariable :: String
rawVariable = "omg"

parsedVariable :: NamedLambda
parsedVariable = Variable "omg"

rawKeywordVariable :: String
rawKeywordVariable = "reset1"

parsedKeywordVariable :: NamedLambda
parsedKeywordVariable = Variable "reset1"

rawAbstraction :: String
rawAbstraction = "λomg.omg"

parsedAbstraction :: NamedLambda
parsedAbstraction = Abstraction "omg" $ Variable "omg"

rawConst :: String
rawConst = "λx.λy.x"

parsedConst :: NamedLambda
parsedConst = Abstraction "x" $ Abstraction "y" $ Variable "x"

rawApplication :: String
rawApplication = "42 42"

parsedApplication :: NamedLambda
parsedApplication = Application (Constant 42) (Constant 42)

rawLongApplication :: String
rawLongApplication = "hello 42 hello"

parsedLongApplication :: NamedLambda
parsedLongApplication = Application (Application (Variable "hello") (Constant 42)) (Variable "hello")

rawParenthesisApplication :: String
rawParenthesisApplication = "hello (world 42)"

parsedParenthesisApplication :: NamedLambda
parsedParenthesisApplication = Application (Variable "hello") (Application (Variable "world") (Constant 42))

rawSum :: String
rawSum = "5 + 5"

parsedSum :: NamedLambda
parsedSum =
  (BinaryOperator Addition
    (Constant 5)
    (Constant 5)
  )

rawMultiplication :: String
rawMultiplication = "5 * 5"

parsedMultiplication :: NamedLambda
parsedMultiplication =
  (BinaryOperator Multiplication
    (Constant 5)
    (Constant 5)
  )

rawSimpleArithmetic :: String
rawSimpleArithmetic = "1+(1)"

parsedSimpleArithmetic :: NamedLambda
parsedSimpleArithmetic =
  (BinaryOperator Addition
    (Constant 1)
    (Constant 1)
  )

rawArithmetic :: String
rawArithmetic = "2 * (1 + (2 - 3) * 2)"

parsedArithmetic :: NamedLambda
parsedArithmetic =
  (BinaryOperator Multiplication
    (Constant 2)
    (BinaryOperator Addition
      (Constant 1)
      (BinaryOperator Multiplication
        (BinaryOperator Subtraction
          (Constant 2)
          (Constant 3)
        )
        (Constant 2)
      )
    )
  )
