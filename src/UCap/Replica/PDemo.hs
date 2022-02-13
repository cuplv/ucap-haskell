{-# LANGUAGE FlexibleContexts #-}

module UCap.Replica.PDemo where

import UCap.Coord
import UCap.Domain
import UCap.Lens
import UCap.Replica
import UCap.Replica.Demo

import Control.Monad.State
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)

type PScript g m = ScriptT g m ()

data RepState g m
  = Idle
  | Running (PScript g m)
  | Waiting (ScriptB g m ())

isIdle :: RepState g m -> Bool
isIdle Idle = True
isIdle _ = False

type PState g m = Map (GId g) (RepState g m)

type PDemo g m = StateT (PState g m) (DemoState g m)

runPDemo
  :: (CoordSys g, Monad m)
  => GState g
  -> RState g
  -> PState g m
  -> PDemo g m a
  -> m ((a, PState g m), RState g)
runPDemo s0 rs0 p0 act =
  runDemo s0 rs0 (runStateT act p0)

evalPDemo
  :: (CoordSys g, Monad m)
  => Map (GId g) (PScript g m)
  -> g
  -> GState g
  -> PDemo g m a
  -> m a
evalPDemo psc0 g0 s0 act =
  fst
  <$> evalDemo
        (Map.keys psc0)
        g0
        s0
        (runStateT act (Map.map Running psc0))

replicaIds :: (Monad m) => PDemo g m [GId g]
replicaIds = Map.keys <$> get

getScript
  :: (CoordSys g, Monad m)
  => GId g
  -> PDemo g m (Maybe (PScript g m))
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
evalRep :: (CoordSys g, Monad m) => GId g -> PDemo g m Bool
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
evalRepB :: (CoordSys g, Monad m) => GId g -> PDemo g m Bool
evalRepB i = do
  r <- evalRep i
  if r
     then broadcast i
     else return ()
  return r

{-| List of replicas which are not in the 'Idle' state. -}
nonIdle :: (CoordSys g, Monad m) => PDemo g m [GId g]
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
loopSeqPD :: (CoordSys g, Monad m) => PDemo g m [GId g]
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
loopPD :: (CoordSys g, Monad m) => PDemo g m [GId g]
loopPD = do
  rids <- replicaIds
  progress <- or <$> traverse evalRep rids
  nonIs <- nonIdle
  if progress && not (null nonIs)
     then traverse_ broadcast rids >> loopPD
     else return nonIs

addScript :: (CoordSys g, Monad m) => GId g -> PScript g m -> PDemo g m ()
addScript i sc = do
  scm <- use $ at i
  case scm of
    Just Idle ->
      at i .= Just (Running sc)
    Just (Running sc0) ->
      at i .= Just (Running $ sc0 >> sc)
    Just (Waiting acs) ->
      at i .= Just (Waiting $ acs `andThen_` sc)

{-| @'unicast i1 i2'@ sends an update from @i1@ to @i2@.  After doing
  so, @i2@ will have seen all events that @i1@ has, and @i2@'s
  coordination information includes that of @i1@. -}
unicast :: (CoordSys g, Monad m) => GId g -> GId g -> PDemo g m ()
unicast send recv = lift $ observeD recv send

{-| Send updates from the given replica to all others. -}
broadcast :: (CoordSys g, Monad m) => GId g -> PDemo g m ()
broadcast i = do
  rids <- filter (/= i) <$> replicaIds
  traverse_ (unicast i) rids

liftPDemo :: (Monad m) => m a -> PDemo g m a
liftPDemo = lift . liftDemo
