module NamedLambdas
  ( Lambda(..)
  , toUnnamedLambda
  , toNamedLambda
  , evaluateByName
  , evaluateByValue
  ) where

import           Data.List
import qualified DeBruijnLambdasInternal as D
import           NamedLambdasInternal

data Lambda i
  = Constant Int
  | Variable VariableName
  | Operator (Lambda i) (Lambda i)
  | Abstraction VariableName (Lambda i)
  | Application (Lambda i) (Lambda i)
  | Position i (Lambda i)
  deriving (Eq)

instance Show (Lambda i) where
  show (Constant value)         = show value
  show (Variable name)          = name
  show (Operator a b)           = show a ++ "+" ++ show b
  show (Abstraction name body)  = "λ" ++ name ++ "." ++ show body
  show (Application lambda arg) = "(" ++ show lambda ++ " " ++ show arg ++ ")"
  show (Position _ body)     = show body

toUnnamedLambda :: (Monad m) => Lambda i -> m (D.Lambda i)
toUnnamedLambda = unname [] where
  unname _ (Constant value) = return $ D.Constant value
  unname context (Variable name) = do
    let (Just newIndex) = elemIndex name context
    return $ D.Variable newIndex
  unname context (Operator a b) = do
    a' <- unname context a
    b' <- unname context b
    return $ D.Operator a' b'
  unname context (Abstraction name body) = do
    body' <- unname (name:context) body
    return $ D.Abstraction body'
  unname context (Application body arg) = do
    body' <- unname context body
    arg' <- unname context arg
    return $ D.Application body' arg'
  unname context (Position info body) = do
    body' <- unname context body
    return $ D.Position info body'

toNamedLambda :: (Monad m) => D.Lambda i -> m (Lambda i)
toNamedLambda = name [] where
  name _ (D.Constant value) = return $ Constant value
  name context (D.Variable index) = do
    let (Just newName) = context ?! index
    return $ Variable newName
  name context (D.Operator a b) = do
    a' <- name context a
    b' <- name context b
    return $ Operator a' b'
  name context (D.Abstraction body) = do
    let newName = findNewName context
    let context' = newName:context
    body' <- name context' body
    return $ Abstraction newName body'
  name context (D.Application body arg) = do
    body' <- name context body
    arg' <- name context arg
    return $ Application body' arg'
  name context (D.Position info body) = do
    body' <- name context body
    return $ Position info body'

evaluateByName :: (Monad m, Show i) => Lambda i -> m (Lambda i)
evaluateByName lambda = do
  unnamved <- toUnnamedLambda lambda
  evaluated <- D.evaluateByName unnamved
  toNamedLambda evaluated

evaluateByValue :: (Monad m, Show i) => Lambda i -> m (Lambda i)
evaluateByValue lambda = do
  unnamed <- toUnnamedLambda lambda
  evaluated <- D.evaluateByValue unnamed
  toNamedLambda evaluated

(?!) :: [a] -> Int -> Maybe a
[] ?! _ = Nothing
(x:xs) ?! n
  | n == 0 = Just x
  | n > 0 = xs ?! (n - 1)
  | otherwise = Nothing
