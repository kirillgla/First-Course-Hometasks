module ParserData.BasicTest where

import BooleanSyntaxTree

rawConstant :: String
rawConstant = "False"

parsedConstant :: BooleanSyntaxTree
parsedConstant = Constant False

rawVariable :: String
rawVariable = "Fasle"

parsedVariable :: BooleanSyntaxTree
parsedVariable = Variable "Fasle"
