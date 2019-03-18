module Variable where

data Variable = Variable { getI :: Int, getJ :: Int, getK :: Int }

isValidVariable :: Variable -> Bool
isValidVariable (Variable i j k) = i `elem` [0..8] && j `elem` [0..8] && k `elem` [1..9]

uniqueCode :: Variable -> Int
uniqueCode variable@(Variable i j k)
  | isValidVariable variable = 81 * i + 9 * j + (k - 1)
  | otherwise = error "Impossible coordinates"

fromCode :: Int -> Variable
fromCode n = result where
  i = n `div` 81
  j = (n `mod` 81) `div` 9
  k  = n `mod` 9
  _result = Variable i j k
  result = if isValidVariable _result then _result else error "Impossible coordinates"
