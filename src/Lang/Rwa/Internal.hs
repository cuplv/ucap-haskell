{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Lang.Rwa.Internal
  ( RwState (..)
  , Block (..)
  , RwaTerm (..)
  , Rwa
  , MonadRwa (..)
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Free
import Lens.Micro.Platform

class RwState w where
  type ReadRep w

instance (RwState a, RwState b) => RwState (a,b) where
  type ReadRep (a,b) = (ReadRep a, ReadRep b)

instance (RwState a, RwState b, RwState c) => RwState (a,b,c) where
  type ReadRep (a,b,c) = (ReadRep a, ReadRep b, ReadRep c)

instance (RwState a, RwState b, RwState c, RwState d) => RwState (a,b,c,d) where
  type ReadRep (a,b,c,d) = (ReadRep a, ReadRep b, ReadRep c, ReadRep d)

instance (RwState a, RwState b, RwState c, RwState d, RwState e) => RwState (a,b,c,d,e) where
  type ReadRep (a,b,c,d,e) = (ReadRep a, ReadRep b, ReadRep c, ReadRep d, ReadRep e)

newtype Block w m a
  = Block { unpackBlock :: ExceptT () (ReaderT (ReadRep w) m) a }

instance (Functor m) => Functor (Block w m) where
  fmap f (Block m) = Block (fmap f m)

instance (Monad m) => Applicative (Block w m) where
  pure = Block . pure
  (Block m1) <*> (Block m2) = Block (m1 <*> m2)

instance (Monad m) => Monad (Block w m) where
  return = pure
  (Block m1) >>= f = Block $ m1 >>= unpackBlock . f

instance MonadTrans (Block w) where
  lift = Block . lift . lift

data RwaTerm w m a
  = ReadState (ReadRep w -> a)
  | WriteState w a
  | Await (Block w m a)

instance (Functor m) => Functor (RwaTerm w m) where
  fmap f sc = case sc of
                ReadState f1 -> ReadState (f . f1)
                WriteState w a  -> WriteState w (f a)
                Await b -> Await $ fmap f b

type Rwa w m = FreeT (RwaTerm w m) m

class (Monad (RwaU m)) => MonadRwa m where
  type RwaS m
  type RwaU m :: * -> *

  {-| 'awaitM' is like 'Lang.Rwa.await', but works cleanly over monad
  transformers like 'Control.Monad.State.StateT'. -}
  awaitM :: Block (RwaS m) (RwaU m) (m a) -> m a

instance (Monad m) => MonadRwa (FreeT (RwaTerm w m) m) where
  type RwaS (FreeT (RwaTerm w m) m) = w
  type RwaU (FreeT (RwaTerm w m) m) = m
  awaitM b = wrap $ Await b

instance (MonadRwa m) => MonadRwa (StateT s m) where
  type RwaS (StateT s m) = RwaS m
  type RwaU (StateT s m) = RwaU m
  awaitM b = StateT $ \s -> awaitM ((\a -> runStateT a s) <$> b)
