{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
module Transformer.Transformer where

import Transformer.BacktrackableListT

class Transformer (t :: (* -> *) -> * -> *) where
  promote :: Monad m => m a -> t m a
  {- if booth functions were total,
   - transformer would be isomorphic to m a,
   - which id defenetly not a desired result,
   - so this function doesn't seem to be meaningful
   -}
  observe :: Monad m => t m a -> m a

instance Transformer BacktrackableListT where
  promote :: Monad m => m a -> BacktrackableListT m a
  promote = BacktrackableListT . (:[])
  observe :: Monad m => BacktrackableListT m a -> m a
  observe = head . runBacktrackableListT
