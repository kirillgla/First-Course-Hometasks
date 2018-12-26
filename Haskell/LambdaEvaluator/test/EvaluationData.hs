module EvaluationData where

import           DeBruijnLambdas
import           LambdaInfo

simpleAdditionBody :: Lambda LambdaInfo
simpleAdditionBody = Operator (Variable 0) (Variable 0)

simpleAddition :: Lambda LambdaInfo
simpleAddition = Application (Abstraction simpleAdditionBody) (Constant 3)

-- (\x y z -> x + y + z) 1 2 3
-- (LLL.2.1.0) 1 2 3
complexLambda :: Lambda LambdaInfo
complexLambda =
  Application
    (Application
      (Application
        (Operator
          (Operator
            (Variable 2)
            (Variable 1)
          )
          (Variable 0)
        )
        (Constant 1)
      )
      (Constant 2)
    )
    (Constant 42)

lambdaWithBadReferences :: Lambda LambdaInfo
lambdaWithBadReferences =
  Application
    (Abstraction
      (Operator
        (Variable 0)
        (Variable 1)
      )
    )
    (Constant 42)

combinatorBody :: Lambda LambdaInfo
combinatorBody =
  Abstraction
    (Application
      (Variable 1)
      (Variable 0)
    )

combinator :: Lambda LambdaInfo
combinator = Abstraction combinatorBody

loopBase :: Lambda LambdaInfo
loopBase = Abstraction $ Application (Variable 0) (Variable 0)

loop :: Lambda LambdaInfo
loop = Application loopBase loopBase

constLambda :: Lambda LambdaInfo
constLambda = Abstraction . Abstraction . Variable $ 1

lazyExpression :: Lambda LambdaInfo
lazyExpression = Application (Application constLambda $ Constant 42) loop

doubleAddition :: Lambda LambdaInfo
doubleAddition =
  Application
    (Application
      (Application
        (Abstraction
          (Abstraction
            (Abstraction
              (Operator
                (Operator
                  (Variable 2)
                  (Variable 1)
                )
                (Variable 0)
              )
            )
          )
        )
        (Constant 100)
      )
      (Constant 10)
    )
    (Constant 1)
