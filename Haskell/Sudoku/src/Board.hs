{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
module Board where

newtype Board a = Board { runBoard :: [[a]] }

class PrettyShow s where
  prettyShow :: s -> String

prettyPrint :: PrettyShow s => s -> IO ()
prettyPrint = putStrLn . prettyShow

instance PrettyShow (Board (Maybe Int)) where
  prettyShow :: Board (Maybe Int) -> String
  prettyShow (Board board) = foldr (\line acc -> prettyShowLine line ++ "\n" ++ acc) "" board where
    prettyShowLine :: [Maybe Int] -> String
    prettyShowLine = foldr (\element acc -> (maybe "_" show element) ++ acc) ""

instance PrettyShow (Board Int) where
  prettyShow :: Board Int -> String
  prettyShow (Board board) = foldr ((++) . (++ "\n") . foldr ((++) . show) "") "" board
