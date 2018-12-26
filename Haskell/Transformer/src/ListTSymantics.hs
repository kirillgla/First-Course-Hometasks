{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
module ListTSymantics where

import Control.Applicative

import Transformer.BacktrackableListT

-- these names are kind of weird. Had to use them to avoid conflicting Prelude
class ListTSymantics (l :: * -> *) where
  singleton :: a -> l a
  bind :: l a -> (a -> l b) -> l b
  failure :: String -> l a
  choice :: l a -> l a -> l a

cons :: ListTSymantics l => a -> l a -> l a
cons = choice . singleton

instance (Traversable m, Monad m) => ListTSymantics (BacktrackableListT m) where
  singleton :: a -> BacktrackableListT m a
  singleton = return
  bind :: BacktrackableListT m a -> (a -> BacktrackableListT m b) -> BacktrackableListT m b
  bind = (>>=)
  failure :: String -> BacktrackableListT m a
  failure = fail
  choice :: BacktrackableListT m a  -> BacktrackableListT m a -> BacktrackableListT m a
  choice = (<|>)
