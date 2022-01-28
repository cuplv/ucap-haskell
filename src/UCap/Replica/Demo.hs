{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}

module UCap.Replica.Demo where

import UCap
import UCap.Lens
import UCap.Op
import UCap.Replica.Capconf
import UCap.Replica.Types
import UCap.Replica.VClock
import UCap.Replica.VThread

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

type RState i c = (VThread i (CEffect c), Map i (Capconf i c))

type DemoState i c m =
       ExceptT ()
         (ReaderT (CState c)
            (StateT (RState i c) m))

transactD :: (Ord i, Cap c, Monad m) => i -> Op c m () b -> DemoState i c m b
transactD = undefined

{-| @observeD i1 i2@ makes events from (and seen by) @i2@ visible to
  @i1@. -}
observeD :: (Ord i, Cap c, Monad m) => i -> i -> DemoState i c m ()
observeD i1 i2 = do
  _1 %= observe i1 i2
  use (_2 . at i2) >>= \case
    Just cc2 -> _2 %= Map.adjust (<> cc2) i1
    Nothing -> return ()

{-| Get the initial state. -}
initialState :: (Monad m) => DemoState i c m (CState c)
initialState = ask

{-| Get an effect representing all updates that have occured, from the
  perspective of one process. -}
allEffectsD :: (Ord i, Cap c, Monad m) => i -> DemoState i c m (CEffect c)
allEffectsD i = mconcat . serialize . reduceToVis i <$> use _1

{-| Read the current state, from the perspective of one process. -}
stateD
  :: (Ord i, Cap c, Monad m)
  => i
  -> DemoState i c m (CState c)
stateD i = eFun <$> allEffectsD i <*> initialState
