module ParserData.OperatorTest where

import BooleanSyntaxTree

rawInversion :: String
rawInversion = "not me"

parsedInversion :: BooleanSyntaxTree
parsedInversion = Inversion $ Variable "me"

rawConjunction :: String
rawConjunction = "andd and aand"

parsedConjunction :: BooleanSyntaxTree
parsedConjunction = Variable "andd" `Conjunction` Variable "aand"

rawDisjunction :: String
rawDisjunction = "False or And"

parsedDisjunction :: BooleanSyntaxTree
parsedDisjunction = Constant False `Disjunction` Variable "And"

rawMalformedConjunction :: String
rawMalformedConjunction = "and False"
