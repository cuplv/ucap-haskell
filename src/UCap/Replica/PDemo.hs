{-# LANGUAGE FlexibleContexts #-}

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
  | Waiting (AwaitBs i c m (PScript i c m))

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

{-| Perform 'evalRep' and then 'broadcast' if any updates were made. -}
evalRepB :: (Ord i, Cap c, Monad m) => i -> PDemo i c m Bool
evalRepB i = do
  r <- evalRep i
  if r
     then broadcast i
     else return ()
  return r

{-| List of replicas which are not in the 'Idle' state. -}
nonIdle :: (Ord i, Monad m) => PDemo i c m [i]
nonIdle = catMaybes <$> (traverse f =<< replicaIds)
  where f i = do
          m <- get
          case Map.lookup i m of
            Just Idle -> return Nothing
            Just _ -> return (Just i)
            Nothing -> error "Replica had no script/state (nonIdle)."

{-| Loop through the replicas, running scripts which are not blocked.
  After each pause in a script, that replica broadcasts.  This means
  that replicas run in a sequential context.

  This continues until all replicas are either finished ('Idle') or
  deadlocked ('Waiting' with no opportunity to resume).  The IDs of
  deadlocked replicas are returned, so an empty list indicates
  successful completion. -}
loopSeqPD :: (Ord i, Cap c, Monad m) => PDemo i c m [i]
loopSeqPD = do
  rids <- replicaIds
  progress <- or <$> traverse evalRepB rids
  if progress
     then loopSeqPD
     else nonIdle

{-| Like 'loopPD', but broadcasts are only sent when all replicas have
  become stuck.  If all replicas terminate, their resulting states
  may be left different from one another.

  This imposes inconsistency.  A replica may act upon a view of
  the state that other replicas do not share. -}
loopPD :: (Ord i, Cap c, Monad m) => PDemo i c m [i]
loopPD = loopPD' True

loopPD' tryB = do
  rids <- replicaIds
  progress1 <- or <$> traverse evalRep rids
  if progress1
     then loopPD' True
     else do is <- nonIdle
             if is /= [] && tryB
                then traverse_ broadcast rids >> loopPD' False
                else return is

addScript :: (Ord i, Monad m) => i -> PScript i c m -> PDemo i c m ()
addScript i sc = do
  scm <- use $ at i
  case scm of
    Just Idle ->
      at i .= Just (Running sc)
    Just (Running sc0) ->
      at i .= Just (Running $ sc0 >> sc)
    Just (Waiting acs) ->
      at i .= Just (Waiting $ map (\(ac,sc0) -> (ac, sc0 >> sc)) acs)

{-| @'unicast i1 i2'@ sends an update from @i1@ to @i2@.  After doing
  so, @i2@ will have seen all events that @i1@ has, and @i2@'s
  coordination information includes that of @i1@. -}
unicast :: (Ord i, Cap c, Monad m) => i -> i -> PDemo i c m ()
unicast send recv = lift $ observeD recv send

{-| Send updates from the given replica to all others. -}
broadcast :: (Ord i, Cap c, Monad m) => i -> PDemo i c m ()
broadcast i = do
  rids <- filter (/= i) <$> replicaIds
  traverse_ (unicast i) rids

liftPDemo :: (Monad m) => m a -> PDemo i c m a
liftPDemo = lift . liftDemo
