-- Thiese methods were moved to separate module to preserve some encapsulation
-- while making it possible to write tests
module DeBruijnLambdasInternal where

-- i is positions info
data Lambda i
  = Constant Int
  | Variable Int
  | Operator (Lambda i) (Lambda i)
  | Abstraction (Lambda i)
  | Application (Lambda i) (Lambda i)
  | Position i (Lambda i)
  deriving (Eq)

instance Show (Lambda i) where
  show (Constant value)              = show value
  show (Variable value)              = '↑':show value
  show (Operator first second)       = show first ++ "+" ++ show second
  show (Abstraction body)            = "λ." ++ show body
  show (Application lambda argument) = "(" ++ show lambda ++ " " ++ show argument ++ ")"
  show (Position _ body)          = show body

-- The operation should be idempotent, i.e.
-- (operation lambda >>= operation) == (operation lambda)
newtype (Monad m) => ArgumentPreprocessor m i =
  ArgumentPreprocessor { getPreprocessor :: Lambda i -> m (Lambda i) }

evaluateByName :: (Monad m, Show i) => Lambda i -> m (Lambda i)
evaluateByName = evaluate Nothing $ ArgumentPreprocessor pure

evaluateByValue :: (Monad m, Show i) => Lambda i -> m (Lambda i)
evaluateByValue = evaluate Nothing $ ArgumentPreprocessor evaluateByValue

evaluate :: (Monad m, Show i) => Maybe i -> ArgumentPreprocessor m i -> Lambda i -> m (Lambda i)
-- Recursive definition base
evaluate _ _ constant@(Constant _) = pure constant
evaluate Nothing _ (Variable _) = fail "Undefined variable"
evaluate (Just position) _ (Variable _) = fail $ "Undefined variable at " ++ show position
evaluate position preprocessor (Operator first second) = do
  evaluatedFirst <- evaluate position preprocessor first
  evaluatedSecond <- evaluate position preprocessor second
  addLambdas position evaluatedFirst evaluatedSecond
evaluate _ _ abstraction@(Abstraction _) = pure abstraction
-- Ready application handling
evaluate position preprocessor (Application (Abstraction body) argument) = do
  newArgument <- getPreprocessor preprocessor argument
  evaluate position preprocessor . decrementIndices . inline body 0 . incrementIndices $ newArgument
-- Potentially correct application handling
evaluate position preprocessor (Application lambda@Application {} argument) = do
  newLambda <- evaluate position preprocessor lambda
  newArgument <- getPreprocessor preprocessor argument
  evaluate position preprocessor $ Application newLambda newArgument
evaluate position preprocessor (Application lambda@Position {} argument) = do
  newLambda <- evaluate position preprocessor lambda
  newArgument <- evaluate position preprocessor argument
  evaluate position preprocessor $ Application newLambda newArgument
-- Invalid application handling
-- this duplicate code looks ugly but helps to avoid unintended fallbacks
evaluate position _ (Application constant@Constant {} argument) =
  applicationError position constant argument
evaluate position _ (Application variable@Variable {} argument) =
  applicationError position variable argument
evaluate position _ (Application operator@Operator {} argument) =
  applicationError position operator argument
evaluate _ preprocessor (Position info body) =
  evaluate (Just info) preprocessor body

-- Inline third argument into first argument
inline :: Lambda i -> Int -> Lambda i -> Lambda i
inline constant@(Constant _) _ _ = constant
inline variable@(Variable index) target value
  | index == target = value
  | otherwise = variable
inline (Operator first second) target value = Operator newFirst newSecond where
    newFirst = inline first target value
    newSecond = inline second target value
inline (Abstraction body) target value =
  Abstraction $ inline body (succ target) $ incrementIndices value
inline (Application lambda argument) target value =
  Application newBody newArgument where
    newBody = inline lambda target value
    newArgument = inline argument target value
inline (Position info body) target value = Position info body' where
  body' = inline body target value

upliftIndices :: Int -> Lambda i -> Int -> Lambda i
upliftIndices _ constant@(Constant _) _ = constant
upliftIndices amount variable@(Variable index) target
  | index >= target = Variable $ index + amount
  | otherwise       = variable
upliftIndices amount (Operator first second) target =
  Operator newFirst newSecond where
    newFirst = upliftIndices amount first target
    newSecond = upliftIndices amount second target
upliftIndices amount (Abstraction body) target = Abstraction reduced where
  reduced = upliftIndices amount body $ target + 1
upliftIndices amount (Application lambda argument) target =
  Application newLambda newArgument where
    newLambda = upliftIndices amount lambda target
    newArgument = upliftIndices amount argument target
upliftIndices amount (Position info body) target = Position info body' where
  body' = upliftIndices amount body target

incrementIndices :: Lambda i -> Lambda i
incrementIndices lambda = upliftIndices 1 lambda 0

decrementIndices :: Lambda i -> Lambda i
decrementIndices lambda = upliftIndices (-1) lambda 0

-- Booth arguments are considered already evaluated
addLambdas :: (Monad m, Show i) => Maybe i -> Lambda i -> Lambda i -> m (Lambda i)
addLambdas _ (Constant first) (Constant second) = pure . Constant $ first + second
addLambdas Nothing first second = fail $ additionErrorMessage first second
addLambdas (Just position) first second = fail $
  additionErrorMessage first second ++ " at " ++ show position

additionErrorMessage :: Lambda i -> Lambda i -> String
additionErrorMessage first second =
  "Could not perform numerical operation betweeen '"
    ++ show  first ++ "' and '" ++ show second ++ "'"

applicationError :: (Monad m, Show i) => Maybe i -> Lambda i -> Lambda i -> m (Lambda i)
applicationError Nothing lambda argument = fail $ "Cannot apply '"
  ++ show lambda ++ "' to '" ++ show argument ++ "'"
applicationError (Just position) lambda argument = fail $ "Cannot apply '"
  ++ show lambda ++ "' to '" ++ show argument ++ "' at " ++ show position
