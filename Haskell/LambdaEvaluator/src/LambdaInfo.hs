module LambdaInfo where

data LambdaInfo = LambdaInfo { getLineNumber :: Int, getColumnNumber :: Int }
  deriving Eq

instance Show LambdaInfo where
  show LambdaInfo { getLineNumber = line, getColumnNumber = column } =
     "line " ++ show line ++ ", column " ++ show column
