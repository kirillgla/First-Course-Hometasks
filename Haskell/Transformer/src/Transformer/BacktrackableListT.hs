{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Transformer.BacktrackableListT where

import Control.Monad
import Control.Applicative
import qualified Control.Monad.Fail as Fail

newtype BacktrackableListT (m :: * -> *) a = BacktrackableListT { runBacktrackableListT :: [m a] } deriving (Eq, Show)

instance Functor m => Functor (BacktrackableListT m) where
  fmap :: (a -> b) -> BacktrackableListT m a -> BacktrackableListT m b
  fmap f = BacktrackableListT . fmap (fmap f) . runBacktrackableListT

instance Applicative m => Applicative (BacktrackableListT m) where
  pure :: a -> BacktrackableListT m a
  pure = BacktrackableListT . pure . pure
  (<*>) :: BacktrackableListT m (a -> b) -> BacktrackableListT m a -> BacktrackableListT m b
  mfs <*> mxs = BacktrackableListT $ (<*>) <$> runBacktrackableListT mfs <*> runBacktrackableListT mxs

instance (Traversable m, Monad m) => Monad (BacktrackableListT m) where
  (>>=) :: BacktrackableListT m a -> (a -> BacktrackableListT m b) -> BacktrackableListT m b
  a >>= k = BacktrackableListT $ runBacktrackableListT a >>= (fmap join . sequenceA . fmap (runBacktrackableListT . k))
  fail :: String -> BacktrackableListT m a
  fail = Fail.fail

instance (Traversable m, Monad m) => Fail.MonadFail (BacktrackableListT m) where
  fail :: String -> BacktrackableListT m a
  fail _ = BacktrackableListT []

instance Applicative m => Alternative (BacktrackableListT m) where
  empty :: BacktrackableListT m a
  empty = BacktrackableListT []
  (<|>) :: BacktrackableListT m a -> BacktrackableListT m a -> BacktrackableListT m a
  a <|> b = BacktrackableListT $ runBacktrackableListT a ++ runBacktrackableListT b

instance (Traversable m, Monad m) => MonadPlus (BacktrackableListT m) where
  mzero :: BacktrackableListT m a
  mzero = empty
  mplus :: BacktrackableListT m a -> BacktrackableListT m a -> BacktrackableListT m a
  mplus = (<|>)
