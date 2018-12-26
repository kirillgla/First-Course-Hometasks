module BinaryOperator where

data BinaryOperator
  = Addition
  | Subtraction
  | Multiplication
  deriving Eq

instance Show BinaryOperator where
  show Addition = "+"
  show Subtraction = "-"
  show Multiplication = "*"

runOperator :: Num n => BinaryOperator -> n -> n -> n
runOperator Addition = (+)
runOperator Subtraction = (-)
runOperator Multiplication = (*)
