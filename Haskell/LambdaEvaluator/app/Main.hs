module Main where

import           Failable
import           LambdaInfo
import           NamedLambdas

main :: IO ()
main = do
  putStrLn ""
  demonstrateLaziness
  demonstrateSilentFailure
  demonstrateFailureWithMessage
  demonstrateFailureWithPosition

demonstrateLaziness :: IO ()
demonstrateLaziness = demonstrate "lazy evaluation" expression result where
  loopBase = Abstraction "x" $ Application (Variable "x") (Variable "x")
  loop = Application loopBase loopBase
  constLambda = Abstraction "a" . Abstraction "b" . Variable $ "a"
  expression = Application (Application constLambda $ Constant 42) loop
  result = evaluateByName expression :: Either String (Lambda LambdaInfo)

demonstrateSilentFailure :: IO ()
demonstrateSilentFailure = demonstrate "silent failure" badLambda result where
  badLambda = Application (Constant 42) (Constant 11)
  result = evaluateByName badLambda :: Maybe (Lambda LambdaInfo)

demonstrateFailureWithMessage :: IO ()
demonstrateFailureWithMessage = demonstrate "failure with message" badLambda result where
  badLambda = Application (Constant 42) (Constant 11)
  result = evaluateByName badLambda :: Failable (Lambda LambdaInfo)

demonstrateFailureWithPosition :: IO ()
demonstrateFailureWithPosition = demonstrate "failure with position info" badLambda result where
  badLambda = Position (LambdaInfo 1 1) $ Application (Position (LambdaInfo 2 1) $ Constant 42) (Constant 11)
  result = evaluateByName badLambda :: Failable (Lambda LambdaInfo)

demonstrate :: (Monad m, Show (m (Lambda i))) => String -> Lambda i -> m (Lambda i) -> IO ()
demonstrate name lambda result = do
  putStr "This example demonstrates "
  putStr name
  putStrLn ":"
  putStr "  - Expression: "
  print lambda
  putStr "  - Result: "
  print result
  putStrLn ""
