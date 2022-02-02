{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module UCap.Replica.PDemo where

import UCap.Domain
import UCap.Lens
import UCap.Replica
import UCap.Replica.Demo

import Control.Monad.State
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)

type PScript i c m = ScriptT i c m ()

data RepState i c m
  = Idle
  | Running (PScript i c m)
  | Waiting (AwaitBs i c (PScript i c m))

isIdle :: RepState i c m -> Bool
isIdle Idle = True
isIdle _ = False

type PState i c m = Map i (RepState i c m)

type PDemo i c m = StateT (PState i c m) (DemoState i c m)

runPDemo
  :: (Ord i, Cap c, Monad m)
  => CState c
  -> RState i c
  -> PState i c m
  -> PDemo i c m a
  -> m ((a, PState i c m), RState i c)
runPDemo s0 rs0 p0 act =
  runDemo s0 rs0 (runStateT act p0)

evalPDemo
  :: (Ord i, Cap c, Monad m)
  => Map i (PScript i c m)
  -> Capconf i c
  -> CState c
  -> PDemo i c m a
  -> m a
evalPDemo psc0 cc0 s0 act =
  fst
  <$> evalDemo
        (Map.keys psc0)
        cc0
        s0
        (runStateT act (Map.map Running psc0))

replicaIds :: (Ord i, Monad m) => PDemo i c m [i]
replicaIds = Map.keys <$> get

getScript
  :: (Ord i, Cap c, Monad m)
  => i
  -> PDemo i c m (Maybe (PScript i c m))
getScript i = do
  scm <- use $ at i
  case scm of
    Just (Running sc) -> return (Just sc)
    Just (Waiting acs) -> lift $ tryAwait i acs
    Just Idle -> return Nothing
    Nothing -> error "Replica had no script/state (getScript)."

{-| Evaluate a replica's script until it either blocks or terminates.
  If any progress was made, 'True' is returned.  If the replica was
  already 'Idle' or stuck in 'Waiting', 'False' is returned. -}
evalRep :: (Ord i, Cap c, Monad m) => i -> PDemo i c m Bool
evalRep i = do
  scm <- getScript i
  case scm of
    Just sc -> do
      r <- lift $ script i sc
      case r of
        Right () -> at i .= Just Idle
        Left acs -> at i .= Just (Waiting acs)
      return True
    _ -> return False

{-| List of replicas which are not in the 'Idle' state. -}
nonIdle :: (Ord i, Monad m) => PDemo i c m [i]
nonIdle = catMaybes <$> (mapM f =<< replicaIds)
  where f i = do
          m <- get
          case Map.lookup i m of
            Just Idle -> return Nothing
            Just _ -> return (Just i)
            Nothing -> error "Replica had no script/state (nonIdle)."

{-| Loop through the replicas, running scripts which are not blocked.
  This continues until all replicas are either finished ('Idle') or
  deadlocked ('Waiting' with no opportunity to resume).  The IDs of
  deadlocked replicas are returned, so an empty list indicates
  successful completion. -}
loopPD :: (Ord i, Cap c, Monad m) => PDemo i c m [i]
loopPD = do
  rids <- replicaIds
  progress <- or <$> mapM evalRep rids
  if progress
     then loopPD
     else nonIdle
