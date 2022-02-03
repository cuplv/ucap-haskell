{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module UCap.Replica.Script
  ( ScriptT
  , ScriptTerm
  , ScriptB
  , RepCtx (..)
  , rsStore
  , rsCapconf
  , rsCoord
  , RepCtx'
  , unwrapScript
  , getReplicaId
  , liftScript
  , transact
  , module Lang.Rwa
  ) where

import Lang.Rwa
import UCap.Domain
import UCap.Lens
import UCap.Op
import UCap.Replica.Capconf
import UCap.Replica.Coord

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Free

data RepCtx i c e
  = RepCtx { _rsStore :: e
           , _rsCapconf :: Capconf i c
           , _rsCoord :: Coord i c
           }

makeLenses ''RepCtx

instance (Ord i, Cap c, Semigroup e) => Semigroup (RepCtx i c e) where
  RepCtx e1 cc1 cd1 <> RepCtx e2 cc2 cd2 =
    RepCtx (e1 <> e2) (cc1 <> cc2) (cd1 <> cd2)

instance (Ord i, Cap c, Monoid e) => Monoid (RepCtx i c e) where
  mempty = RepCtx mempty mempty mempty

instance (EffectDom e) => RwState (RepCtx i c e) where
  type ReadRep (RepCtx i c e) = RepCtx i c (EDState e)

type RepCtx' i c = RepCtx i c (CEffect c)

type ScriptT i c m a = Rwa (RepCtx' i c) (ReaderT i m) a

type ScriptTerm i c m a = RwaTerm (RepCtx' i c) (ReaderT i m) a

type ScriptB i c m a = AwaitB (RepCtx' i c) (ReaderT i m) a

getReplicaId :: (Monad m) => ScriptT i c m i
getReplicaId = ask

{-| Compile an operation into a replica script. -}
transact
  :: (Ord i, Cap c, Monad m)
  => Op c m () a
  -> ScriptT i c m (Maybe a)
transact op = do
  rid <- getReplicaId
  ctx <- readState
  let s = ctx ^. rsStore
  let cc = ctx ^. rsCapconf
  let caps = Caps (remoteG' rid cc) (localG rid cc)
  case execWith caps s op of
    Just act -> do
      (_,e,b) <- liftScript act
      case consumeG rid e cc of
        Just cc' -> do
          let ctx' = ctx
                & rsStore .~ e
                & rsCapconf .~ cc'
          writeState ctx'
          return (Just b)
    Nothing -> return Nothing

{-| Perform an action on the underlying monad of the script. -}
liftScript :: (Monad m) => m a -> ScriptT i c m a
liftScript = lift . lift

{-| Get the first term of a script, wrapped in 'Left', or the return
  value of the script, wrapped in 'Right'. -}
unwrapScript
  :: (Monad m)
  => ScriptT i c m a
  -> i
  -> m (Either (ScriptTerm i c m a) a)
unwrapScript sc i = runReaderT (nextTerm sc) i
