{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module UCap.Replica.Script
  ( ScriptT
  , ScriptTerm
  , ScriptB
  , RepCtx (..)
  , rsStore
  , rsCoord
  , RepCtx'
  , unwrapScript
  , getReplicaId
  , writeGE
  , emitEffect
  , setCoord
  , onCoord
  , liftScript
  , module Lang.Rwa
  ) where

import Lang.Rwa
import Lang.Rwa.Interpret
import UCap.Coord
import UCap.Domain
import UCap.Lens
import UCap.Op

import Control.Monad.Reader

data RepCtx g e
  = RepCtx { _rsStore :: e
           , _rsCoord :: g
           }
  deriving (Show,Eq,Ord)

makeLenses ''RepCtx

instance (Semigroup g, Semigroup e) => Semigroup (RepCtx g e) where
  RepCtx e1 g1 <> RepCtx e2 g2 =
    RepCtx (e1 <> e2) (g1 <> g2)

instance (Monoid g, Monoid e) => Monoid (RepCtx g e) where
  mempty = RepCtx mempty mempty

instance (EffectDom e) => RwState (RepCtx (Maybe g) e) where
  type ReadRep (RepCtx (Maybe g) e) = RepCtx g (EDState e)

type RepCtx' g = RepCtx (Maybe g) (GEffect g)

type ScriptT g m a = Rwa (RepCtx' g) (ReaderT (GId g) m) a

type ScriptTerm g m a = RwaTerm (RepCtx' g) (ReaderT (GId g) m) a

type ScriptB g m a = Block (RepCtx' g) (ReaderT (GId g) m) (ScriptT g m a)

getReplicaId :: (MonadReader i m) => m i
getReplicaId = ask

writeGE :: (Monad m) => g -> GEffect g -> ScriptT g m ()
writeGE g e = writeState $ RepCtx e (Just g)

emitEffect :: (Monad m) => GEffect g -> ScriptT g m ()
emitEffect e = writeState $ RepCtx e Nothing

setCoord :: (CoordSys g, Monad m) => g -> ScriptT g m ()
setCoord g = writeState $ RepCtx mempty (Just g)

onCoord
  :: (CoordSys g, Monad m)
  => (g -> g)
  -> ScriptT g m ()
onCoord f = setCoord . f . view rsCoord =<< readState

{-| Perform an action on the underlying monad of the script. -}
liftScript :: (Monad m) => m a -> ScriptT g m a
liftScript = lift . lift

{-| Get the first term of a script, wrapped in 'Left', or the return
  value of the script, wrapped in 'Right'. -}
unwrapScript
  :: (Monad m)
  => ScriptT g m a
  -> (GId g)
  -> m (Either (ScriptTerm g m (ScriptT g m a)) a)
unwrapScript sc i = runReaderT (nextTerm sc) i
