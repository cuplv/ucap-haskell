{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module UCap.Replica.Demo
  ( DemoState
  , RState
  , runDemo
  , evalDemo
  , script
  , tryAwait
  , (.//)
  , noBlock
  , observeD
  , stateD
  , coordL
  , liftDemo
  ) where

import Lang.Rwa
import Lang.Rwa.Interpret
import UCap
import UCap.Coord
import UCap.Lens
import UCap.Op
import UCap.Replica.Script
import UCap.Replica.Transact
import UCap.Replica.Types
import UCap.Replica.VClock
import UCap.Replica.VThread

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

data RState g
  = RState { _rsThreads :: VThread (GId g) (GEffect g)
           , _rsCoords :: Map (GId g) g
           , _rsSaveState :: GState g
           , _rsAllIds :: [GId g]
           }

makeLenses ''RState

{-| A monad for simulating transactions on a network of store replicas. -}
type DemoState g m = StateT (RState g) m

{-| Run a demo action on the given initial store state (@'GState' g@) and
  starting network state (@'RState' g@). -}
runDemo
  :: (Monad m)
  => RState g
  -> DemoState g m a
  -> m (a, RState g)
runDemo rs0 act =
  runStateT act rs0

{-| Evaluate the result of a demo action from an initial store state and
  an inital network state defined by a list of process IDs (@[i]@) and
  an initial coordination system state known by all processes
  (@g@). -}
evalDemo
  :: (CoordSys g, Monad m)
  => [GId g]
  -> g
  -> GState g
  -> DemoState g m a
  -> m a
evalDemo ps g0 s0 act = do
  let gs = Map.fromList $ zip ps (repeat g0)
      rs = RState { _rsThreads = initThreads
                  , _rsCoords = gs
                  , _rsSaveState = s0
                  , _rsAllIds = ps
                  }
  fst <$> runDemo rs act

{-| Run a replica script, on a particular replica, in the demo
  simulation. -}
script
  :: (CoordSys g, Monad m)
  => (GId g)
  -> ScriptT g m a
  -> DemoState g m (Either (Block' (ScriptT g m) a) a)
script i sc = liftDemo (unwrapScript sc i) >>= \case
  Left (ReadState f) ->
    script i . f =<< assembleReadState i
  Left (WriteState ctx sc') ->
    applyWriteState i ctx >> script i sc'
  Left (Await acs) -> tryAwait i acs >>= \case
    Just sc' -> script i sc'
    Nothing -> return (Left acs)
  Right a -> return (Right a)

tryAwait
  :: (CoordSys g, Monad m)
  => GId g
  -> Block' (ScriptT g m) a
  -> DemoState g m (Maybe (ScriptT g m a))
tryAwait i b = do
  state <- assembleReadState i
  liftDemo $ runReaderT (checkBlock b state) i

assembleReadState
  :: (CoordSys g, Monad m)
  => GId g
  -> DemoState g m (ReadRep (RepCtx' g))
assembleReadState i = RepCtx <$> stateD i <*> use (coordL i)

applyWriteState
  :: (CoordSys g, Monad m)
  => GId g
  -> RepCtx' g
  -> DemoState g m ()
applyWriteState i (RepCtx e g) = do
  rsThreads %= event i e
  pruneD
  case g of
    Just g -> coordL i %= (<> g)
    Nothing -> return ()

{-| Run a transaction, on a particular replica, in the demo simulation. -}
(.//)
  :: (CoordSys g, Monad m)
  => GId g
  -> Op (GCap g) m () a
  -> DemoState g m (Either
                      (Block' (ScriptT g m) (Either g a))
                      (Either g  a))
i .// op = script i (transactSimple op)

{-| Run a replica script that is not expected to block.  If it does
  block, a runtime error will be thrown. -}
noBlock :: (Monad m) => DemoState g m (Either e a) -> DemoState g m a
noBlock m = m >>= \case
                     Right a -> return a
                     _ -> error "Got Left value under noBlock."

{-| @'observeD' i1 i2@ makes events from (and seen by) @i2@ visible to
  @i1@. -}
observeD :: (CoordSys g, Monad m) => GId g -> GId g -> DemoState g m ()
observeD i1 i2 = do
  -- Update effect log
  rsThreads %= observe i1 i2
  pruneD

  -- Update coord structure
  cd2 <- use $ coordL i2
  coordL i1 %= (<> cd2)

-- {-| Get the initial state. -}
-- initialState :: (Monad m) => DemoState g m (GState g)
-- initialState = ask

{-| Get an effect representing all updates that have occured, from the
  perspective of one process. -}
allEffectsD :: (CoordSys g, Monad m) => GId g -> DemoState g m (GEffect g)
allEffectsD i = mconcat . serialize . reduceToVis i <$> use rsThreads

{-| Read the current state, from the perspective of one process. -}
stateD :: (CoordSys g, Monad m) => GId g -> DemoState g m (GState g)
stateD i = eFun <$> allEffectsD i <*> use rsSaveState

pruneD :: (CoordSys g, Monad m) => DemoState g m ()
pruneD = do
  ids <- use rsAllIds
  t <- use rsThreads
  let (td, t') = prune ids t
  rsThreads .= t'
  let ed = mconcat . serialize $ td
  rsSaveState %= eFun ed

{-| A lens to the 'CoordSys' for a process ID. -}
coordL :: (Ord (GId g)) => GId g -> Lens' (RState g) g
coordL i = rsCoords . at i . nonCheat

liftDemo :: (Monad m) => m a -> DemoState g m a
liftDemo = lift
