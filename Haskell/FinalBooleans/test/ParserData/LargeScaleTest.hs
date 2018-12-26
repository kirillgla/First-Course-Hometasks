module ParserData.LargeScaleTest where

import BooleanSyntaxTree

rawImplication :: String
rawImplication = "not a or b"

parsedImplication :: BooleanSyntaxTree
parsedImplication = Inversion (Variable "a") `Disjunction` Variable "b"
