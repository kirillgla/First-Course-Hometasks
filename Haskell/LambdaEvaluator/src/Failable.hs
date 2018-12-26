module Failable where

newtype Failable a = Failable { fromFailable :: Either String a }

instance Show a => Show (Failable a) where
  show (Failable (Right value)) = show value
  show (Failable (Left message)) = "Error: " ++ message

instance Functor Failable where
  fmap f ex = Failable $ fmap f (fromFailable ex)

instance Applicative Failable where
  pure = Failable . pure
  f <*> x = Failable $ fromFailable f <*> fromFailable x

instance Monad Failable where
  fail = Failable . Left
  x >>= f = Failable $ fromFailable x >>= fromFailable . f
