{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module UCap.Replica.Demo
  ( DemoState
  , RState
  , runDemo
  , evalDemo
  , evalDemoU
  , evalDemoU'
  , script
  , (./$)
  , transactD
  , observeD
  , stateD
  , transferD
  , acceptD
  , maskD
  , capsL
  , liftDemo
  ) where

import UCap
import UCap.Lens
import UCap.Op
import UCap.Replica.Capconf
import UCap.Replica.Script
import UCap.Replica.Types
import UCap.Replica.VClock
import UCap.Replica.VThread

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
-- import Control.Monad.Trans.Free
import Data.Map (Map)
import qualified Data.Map as Map

type RState i c = (VThread i (CEffect c), Map i (Capconf i c))

{-| A monad for simulating transactions on a network of store replicas. -}
type DemoState i c m =
       ExceptT ()
         (ReaderT (CState c)
            (StateT (RState i c) m))

{-| Run a demo action on the given initial store state (@'CState' c@) and
  starting network state (@'RState' i c@). -}
runDemo
  :: (Ord i, Cap c, Monad m)
  => CState c
  -> RState i c
  -> DemoState i c m a
  -> m (Either () a, RState i c)
runDemo s0 rs0 act =
  runStateT (runReaderT (runExceptT act) s0) rs0

{-| Evaluate the result of a demo action from an initial store state and
  an inital network state defined by a list of process IDs (@[i]@) and
  an initial capability configuration known by all processes
  (@'Capconf' i c@). -}
evalDemo
  :: (Ord i, Cap c, Monad m)
  => [i]
  -> Capconf i c
  -> CState c
  -> DemoState i c m a
  -> m (Either () a)
evalDemo ps cc0 s0 act = do
  let ccs = Map.fromList $ zip ps (repeat cc0)
  fst <$> runDemo s0 (initThreads,ccs) act

{-| Same as 'evalDemo', but assign all processes the same initial
  capability. -}
evalDemoU
  :: (Ord i, Cap c, Monad m)
  => [i]
  -> c
  -> CState c
  -> DemoState i c m a
  -> m (Either () a)
evalDemoU ps c0 = evalDemo ps (mkUniform c0 ps)

evalDemoU'
  :: (Ord i, Cap c)
  => [i]
  -> c
  -> CState c
  -> DemoState i c Identity a
  -> a
evalDemoU' ps c0 s0 act = case evalDemo ps (mkUniform c0 ps) s0 act of
  Identity (Right a) -> a

script
  :: (Ord i, Cap c, Monad m)
  => i
  -> ScriptT i c m a
  -> DemoState i c m (Either (ScriptT i c m a) a)
script i sc = liftDemo (unwrapScript sc i) >>= \case
  Left (ReadCaps f) -> script i . f =<< use (capsL i)
  Left (ReadState f) -> script i . f =<< stateD i
  Left (WriteCaps cc' sc') -> (capsL i %= (<> cc')) >> script i sc'
  Left (WriteState e sc') -> (_1 %= event i e) >> script i sc'
  Left (Blocked sc') -> return (Left sc')
  Right a -> return (Right a)

(./$)
  :: (Ord i, Cap c, Monad m)
  => i
  -> ScriptT i c m a
  -> DemoState i c m (Either (ScriptT i c m a) a)
(./$) = script

-- script i sc = case sc of
--   EmitEffect e sc' -> (_1 %= event i e) >> script i sc'
--   ReadCaps f -> script i . f =<< use (capsL i)
--   ReadState f -> script i . f =<< stateD i
--   WriteCaps cc' sc' -> (capsL i .= cc') >> script i sc'
--   Blocked sc' -> return (Left sc')
--   Return a -> return (Right a)

{-| Run an 'Op' on the given process.  If the process does not have
  sufficient capabilities, a 'Nothing' value is returned. -}
transactD
  :: (Ord i, Cap c, Monad m)
  => i
  -> Op c m () b
  -> DemoState i c m (Maybe b)
transactD i op = do
  cc <- use (capsL i)
  let caps = Caps (remoteG' i cc) (localG i cc)
  s <- stateD i
  case execWith caps s op of
    Just act -> do
      (_,e,b) <- liftDemo act
      case consumeG i e cc of
        Just cc' -> do
          _1 %= event i e
          capsL i .= cc'
          return (Just b)
    Nothing -> return Nothing

{-| @'observeD' i1 i2@ makes events from (and seen by) @i2@ visible to
  @i1@. -}
observeD :: (Ord i, Cap c, Monad m) => i -> i -> DemoState i c m ()
observeD i1 i2 = do
  _1 %= observe i1 i2
  use (_2 . at i2) >>= \case
    Just cc2 -> _2 %= Map.adjust (<> cc2) i1
    Nothing -> return ()

{-| @'maskD' i1 (i2,c)@ applies @c@ as a mask to @i1@'s capability, on
  request from @i2@. -}
maskD :: (Ord i, Cap c, Monad m) => i -> (i,c) -> DemoState i c m ()
maskD i1 (i2,c) = capsL i1 %= maskG i1 (i2,c)

{-| @'transferD' i1 (i2,c)@ transfers the capability @c@ from @i1@ to
  @i2@.  If @i1@'s capabilities are not sufficient to do this, an
  exception is thrown in the underying 'ExceptT' monad.

@
foo :: ('Ord' i, 'Cap' c, 'Monad' m) => 'DemoState' i c m 'String'
foo = do
  result <- 'runExceptT' $ 'transferD' i1 (i2,c)
  case result of
    'Right' () -> do
      'acceptD' i2
      'return' "The transfer succeeded."
    'Left' () -> 'return' "The transfer failed."
@
-}
transferD :: (Ord i, Cap c, Monad m)
  => i
  -> (i,c)
  -> DemoState i c (ExceptT () m) ()
transferD i1 (i2,c) = do
  cc <- use $ capsL i1
  case transferG i1 (i2,c) cc of
    Just cc' -> capsL i1 .= cc'
    Nothing -> throwError ()

{-| Accept transferred capabilities on the given process. -}
acceptD :: (Ord i, Cap c, Monad m) => i -> DemoState i c m ()
acceptD i = capsL i %= acceptG i

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

{-| A lens to the 'Capconf' for a process ID. -}
capsL :: (Ord i, Cap c) => i -> Lens' (RState i c) (Capconf i c)
capsL i = _2 . at i . non mempty

liftDemo :: (Monad m) => m a -> DemoState i c m a
liftDemo = lift . lift . lift
