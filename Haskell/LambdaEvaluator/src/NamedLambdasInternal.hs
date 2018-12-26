module NamedLambdasInternal where

type VariableName = String

findNewName :: [VariableName] -> VariableName
findNewName context = tryNewName 0 where
  tryNewName :: Int -> VariableName
  tryNewName index
    | nameToTry `notElem` context = nameToTry
    | otherwise = tryNewName $ index + 1
    where nameToTry = 'v':show index
