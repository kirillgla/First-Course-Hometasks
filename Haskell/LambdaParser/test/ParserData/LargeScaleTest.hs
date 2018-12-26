module ParserData.LargeScaleTest where

import Lambda

rawLazinessDemo :: String
rawLazinessDemo = "(λx.λy.y) ((λx.x x) (λx.x x)) 42"

parsedLazinessDemo :: NamedLambda
parsedLazinessDemo =
  (Application
    (Application
      (Abstraction "x"
        (Abstraction "y"
          (Variable "y")
        )
      )
      (Application
        (Abstraction "x"
          (Application
            (Variable "x")
            (Variable "x")
          )
        )
        (Abstraction "x"
          (Application
            (Variable "x")
            (Variable "x")
          )
        )
      )
    )
    (Constant 42)
  )