{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
module SyntaxTree where

import Transformer.BacktrackableListT
import ListTSymantics

data SyntaxTree (m :: * -> *) b
  = Singleton b
  | forall a. (SyntaxTree m a) :>>=: (a -> SyntaxTree m b)
  | Failure String
  | SyntaxTree m b :|: SyntaxTree m b

instance (Traversable m, Monad m) => ListTSymantics (SyntaxTree m) where
  singleton :: a -> SyntaxTree m a
  singleton = Singleton
  bind :: SyntaxTree m a -> (a -> SyntaxTree m b) -> SyntaxTree m b
  bind = (:>>=:)
  failure :: String -> SyntaxTree m a
  failure = Failure
  choice :: SyntaxTree m a -> SyntaxTree m a -> SyntaxTree m a
  choice = (:|:)

-- pretty much a fold
finalize :: (Traversable m, Monad m, ListTSymantics s) => SyntaxTree m a -> s a
finalize (Singleton x) = singleton x
finalize (x :>>=: f) = finalize x `bind` (finalize . f)
finalize (Failure msg) = failure msg
finalize (left :|: right) = finalize left `choice` finalize right

-- Selects interpretation of final term
initialize :: SyntaxTree m a -> SyntaxTree m a
initialize = id

interpret :: (Traversable m, Monad m) => SyntaxTree m a -> BacktrackableListT m a
interpret = finalize
