{-# LANGUAGE AllowAmbiguousTypes #-}

module UCap.Replica.Demo where

import UCap
import UCap.Op
import UCap.Replica.Types
import UCap.Replica.VClock
import UCap.Replica.VThread

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

type DemoState i c m =
       ExceptT ()
         (ReaderT (CState c)
            (StateT (VThread i (CEffect c)) m))

transactD :: (Ord i, Cap c, Monad m) => i -> Op c m () b -> DemoState i c m b
transactD = undefined

observeD :: (Ord i, Cap c, Monad m) => i -> i -> DemoState i c m ()
observeD = undefined

stateD :: (Ord i, Cap c, Monad m) => i -> DemoState i c m ()
stateD = undefined
