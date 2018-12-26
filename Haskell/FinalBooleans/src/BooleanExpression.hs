{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
module BooleanExpression where

class BooleanExpression b where
  constant :: Bool -> b
  inversion :: b -> b
  conjunction :: b -> b -> b
  disjunction :: b -> b -> b

instance BooleanExpression Bool where
  constant :: Bool -> Bool
  constant = id
  inversion :: Bool -> Bool
  inversion = not
  conjunction :: Bool -> Bool -> Bool
  conjunction = (&&)
  disjunction :: Bool -> Bool -> Bool
  disjunction = (||)

evaluate :: Bool -> Bool
evaluate = id

instance BooleanExpression String where
  constant :: Bool -> String
  constant = show
  inversion :: String -> String
  inversion value = "(not " ++ value ++ ")"
  conjunction :: String -> String -> String
  conjunction left right = "(" ++ left ++ " and " ++ right ++ ")"
  disjunction :: String -> String -> String
  disjunction left right = "(" ++ left ++ " or " ++ right ++ ")"

view :: String -> String
view = id