{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

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

type RState g = (VThread (GId g) (GEffect g), Map (GId g) g)
-- type RState i c = (VThread i (CEffect c), Map i (Capconf i c), Map i (Coord i c))

{-| A monad for simulating transactions on a network of store replicas. -}
type DemoState g m =
       ReaderT (GState g)
         (StateT (RState g) m)

{-| Run a demo action on the given initial store state (@'GState' g@) and
  starting network state (@'RState' g@). -}
runDemo
  :: (Monad m)
  => GState g
  -> RState g
  -> DemoState g m a
  -> m (a, RState g)
runDemo s0 rs0 act =
  runStateT (runReaderT act s0) rs0

{-| Evaluate the result of a demo action from an initial store state and
  an inital network state defined by a list of process IDs (@[i]@) and
  an initial coordination system state known by all processes
  (@g@). -}
evalDemo
  :: (Ord (GId g), CoordSys g, Monad m)
  => [GId g]
  -> g
  -> GState g
  -> DemoState g m a
  -> m a
evalDemo ps g0 s0 act = do
  let gs = Map.fromList $ zip ps (repeat g0)
  -- let cds = Map.fromList $ zip ps (repeat cd0)
  fst <$> runDemo s0 (initThreads,gs) act

-- {-| Same as 'evalDemo', but assign all processes the same initial
--   capability. -}
-- evalDemoU
--   :: (Ord i, Cap c, Monad m)
--   => [i]
--   -> c
--   -> CState c
--   -> DemoState i c m a
--   -> m a
-- evalDemoU ps c0 = evalDemo ps (mkUniform c0 ps) mempty

-- evalDemoU'
--   :: (Ord i, Cap c)
--   => [i]
--   -> c
--   -> CState c
--   -> DemoState i c Identity a
--   -> a
-- evalDemoU' ps c0 s0 act = case evalDemo ps (mkUniform c0 ps) mempty s0 act of
--   Identity a -> a

{-| Run a replica script, on a particular replica, in the demo
  simulation. -}
script
  :: (Ord (GId g), CoordSys g, Monad m)
  => (GId g)
  -> ScriptT g m a
  -> DemoState g m (Either (ScriptB g m a) a)
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
  :: (Ord (GId g), CoordSys g, Monad m)
  => GId g
  -> ScriptB g m a
  -> DemoState g m (Maybe (ScriptT g m a))
tryAwait i b = do
  state <- assembleReadState i
  liftDemo $ runReaderT (checkBlock b state) i

assembleReadState
  :: (Ord (GId g), CoordSys g, Monad m)
  => GId g
  -> DemoState g m (ReadRep (RepCtx' g))
assembleReadState i = RepCtx <$> stateD i <*> use (coordL i)
-- assembleReadState i = do
--   sval <- stateD i
--   g <- use (coordL i)
--   return $ RepCtx sval g

applyWriteState
  :: (Ord (GId g), CoordSys g, Monad m)
  => GId g
  -> RepCtx' g
  -> DemoState g m ()
applyWriteState i (RepCtx e g) = do
  _1 %= event i e
  case g of
    Just g -> coordL i %= (<> g)
    Nothing -> return ()

{-| Run a transaction, on a particular replica, in the demo simulation. -}
(.//)
  :: (Ord (GId g), CoordSys g, Monad m)
  => GId g
  -> Op (GCap g) m () a
  -> DemoState g m (Either
                      (ScriptB g m (Either g a))
                      (Either g  a))
i .// op = script i (transactSimple op)

{-| Run a replica script that is not expected to block.  If it does
  block, a runtime error will be thrown. -}
noBlock :: (Monad m) => DemoState g m (Either e a) -> DemoState g m a
noBlock m = m >>= \case
                     Right a -> return a
                     _ -> error "Got Left value under noBlock."

-- {-| Run an 'Op' on the given process.  If the process does not have
--   sufficient capabilities, a 'Nothing' value is returned. -}
-- transactD
--   :: (Ord i, Cap c, Monad m)
--   => i
--   -> Op c m () b
--   -> DemoState i c m (Maybe b)
-- transactD i op = do
--   cc <- use (capsL i)
--   let caps = Caps (remoteG' i cc) (localG i cc)
--   s <- stateD i
--   case execWith caps s op of
--     Just act -> do
--       (_,e,b) <- liftDemo act
--       case consumeG i e cc of
--         Just cc' -> do
--           _1 %= event i e
--           capsL i .= cc'
--           return (Just b)
--     Nothing -> return Nothing

{-| @'observeD' i1 i2@ makes events from (and seen by) @i2@ visible to
  @i1@. -}
observeD
  :: (Ord (GId g), CoordSys g, Monad m)
  => GId g
  -> GId g
  -> DemoState g m ()
observeD i1 i2 = do
  -- Update effect log
  _1 %= observe i1 i2

  -- -- Update capabilities
  -- cc2 <- use $ capsL i2
  -- capsL i1 %= (<> cc2)

  -- Update coord structure
  cd2 <- use $ coordL i2
  coordL i1 %= (<> cd2)

-- {-| @'maskD' i1 (i2,c)@ applies @c@ as a mask to @i1@'s capability, on
--   request from @i2@. -}
-- maskD :: (Ord i, Cap c, Monad m) => i -> (i,c) -> DemoState i c m ()
-- maskD i1 (i2,c) = capsL i1 %= maskG i1 (i2,c)

-- {-| @'transferD' i1 (i2,c)@ transfers the capability @c@ from @i1@ to
--   @i2@.  If @i1@'s capabilities are not sufficient to do this, an
--   exception is thrown in the underying 'ExceptT' monad.

-- @
-- foo :: ('Ord' i, 'Cap' c, 'Monad' m) => 'DemoState' i c m 'String'
-- foo = do
--   result <- 'runExceptT' $ 'transferD' i1 (i2,c)
--   case result of
--     'Right' () -> do
--       'acceptD' i2
--       'return' "The transfer succeeded."
--     'Left' () -> 'return' "The transfer failed."
-- @
-- -}
-- transferD :: (Ord i, Cap c, Monad m)
--   => i
--   -> (i,c)
--   -> DemoState i c m ()
-- transferD i1 (i2,c) = do
--   cc <- use $ capsL i1
--   case transferG i1 (i2,c) cc of
--     Just cc' -> capsL i1 .= cc'
--     Nothing -> error "Can't transfer."

-- {-| Accept transferred capabilities on the given process. -}
-- acceptD :: (Ord i, Cap c, Monad m) => GId g -> DemoState g m ()
-- acceptD i = capsL i %= acceptG i

{-| Get the initial state. -}
initialState :: (Monad m) => DemoState g m (GState g)
initialState = ask

{-| Get an effect representing all updates that have occured, from the
  perspective of one process. -}
allEffectsD
  :: (Ord (GId g), CoordSys g, Monad m)
  => GId g
  -> DemoState g m (GEffect g)
allEffectsD i = mconcat . serialize . reduceToVis i <$> use _1

{-| Read the current state, from the perspective of one process. -}
stateD
  :: (Ord (GId g), CoordSys g, Monad m)
  => GId g
  -> DemoState g m (GState g)
stateD i = eFun <$> allEffectsD i <*> initialState

{-| A lens to the 'CoordSys' for a process ID. -}
coordL :: (Ord (GId g)) => GId g -> Lens' (RState g) g
coordL i = _2 . at i . nonCheat

liftDemo :: (Monad m) => m a -> DemoState g m a
liftDemo = lift . lift
