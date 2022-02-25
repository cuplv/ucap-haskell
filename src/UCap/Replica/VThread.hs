{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module UCap.Replica.VThread
  ( VThread
  , Event
  , EventId
  , initThreads
    -- * Modification
  , event
  , eventImport
  , eventImport'
  , EventImportError (..)
  , updateClock
  , UpdateClockError (..)
  , observe
  , mergeThread
  , reduceToVis
    -- * Serialize
  , serialize
  , serialize'
  , simpleIdOrder
  , prune
    -- * Query
  , totalClock
  , meetClock
  , getClock
  , getThread
  , lookupEvent
  , precedesE
  , sizeVT
  ) where

import UCap.Domain.Classes (meet)
import UCap.Replica.Types
import UCap.Replica.VClock

import Control.Monad (foldM)
import Data.Aeson
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Merge.Lazy
import Data.Maybe (fromJust)
import GHC.Generics

data VThread i d
  -- Each thread holds a main clock and a list of events.  The main
  -- clock gives the highest clock number for every other thread that
  -- has been witnessed by that thread.
  = VThread (Map i (VClock i, [Event i d]))
  deriving (Show,Eq,Ord,Generic)

instance (ToJSON i, ToJSONKey i, ToJSON d) => ToJSON (VThread i d)
instance (Ord i, FromJSON i, FromJSONKey i, FromJSON d) => FromJSON (VThread i d)

{-| An event with a payload.  The event's clock gives the sequence
  numbers of the latest event of every process (including this event's
  process) that had been observed when this event was created. -}
type Event i d = (VClock i, d)

type EventId i = (i, Int)

instance (Ord i) => IsKV (VThread i d) where
  type DKey (VThread i d) = EventId i
  type DVal (VThread i d) = d

instance (Monad m, Ord i) => Dag (VThread i d) m where
  descendant v i1 i2 = return $ precedesE v i1 i2
  parents i t@ (VThread m) = return $ toList . fst <$> lookupEvent i t
  payload i t = return $ snd <$> lookupEvent i t
  ends t@(VThread m) = narrow t $ f <$> Map.assocs m
    where f (i,(_,es)) = (i, length es - 1)

mlast [] = Nothing
mlast (a : []) = Just a
mlast (a : as) = mlast as

mfirst [] = Nothing
mfirst (a : _) = Just a

initThreads :: VThread i d
initThreads = VThread Map.empty

{-| @'precedesE' t e1 e2@ checks whether the event with ID @e1@ is in
  the causal closure of the event with ID @e2@. -}
precedesE :: (Ord i) => VThread i d -> EventId i -> EventId i -> Bool
precedesE t i1 i2 = case lookupEvent i2 t of
  Just (v2,_) -> case lookupVC (fst i1) v2 of
                   Just n -> (snd i1) <= n
                   Nothing -> False
  _ -> False

{-| Add a new event to a thread. -}
event :: (Ord i) => i -> d -> VThread i d -> VThread i d
event i d (VThread m) = VThread $ Map.alter
  (\a -> 
     let (v,es) = case a of
                    Just (v,es) -> (v,es)
                    Nothing -> (zeroClock,[])
     in Just (tick i v, es ++ [(v,d)]))
  i
  m

{-| Error cases for 'eventImport'. -}
data EventImportError d
  = PayloadConflict d d
  | NotCausal
  | IncompleteClock
  deriving (Show,Eq,Ord)

{-| Add a new event, with designated clock, to a thread.  There are
  several error conditions.

  @('PayloadConflict' d1 d2)@ indicates that an event with the same ID
  already existed and had payload @d2@, which is not equal to the
  received event's payload @d1@.

  'NotCausal' indicates that the event's clock includes events which are not yet included in the dag.

  'IncompleteClock' means that the event's clock precedes the header
  clock for that process in the dag.
-}
eventImport
  :: (Ord i, Eq d)
  => i
  -> (VClock i, d)
  -> VThread i d
  -> Either (EventImportError d) (VThread i d)
eventImport i (v,d) t@(VThread m)
  | v == getClock i t = Right $ event i d t
  | v `precedes` getClock i t = Right t
  | otherwise = Left NotCausal

{-| First runs 'updateClock' with the new event's clock, and then tries
  'eventImport'. -}
eventImport'
  :: (Ord i, Eq d)
  => i
  -> (VClock i, d)
  -> VThread i d
  -> Either (EventImportError d) (VThread i d)
eventImport' i (v,d) t = case updateClock i v t of
  Right t' -> eventImport i (v,d) t'
  Left OldValues -> eventImport i (v,d) t
  Left MissingEvents -> Left NotCausal

clockToId :: (Ord i) => i -> VClock i -> EventId i
clockToId i v = case lookupVC i v of
                  Just n -> (i, n + 1)
                  Nothing -> (i, 0)

{-| Get an event referred to by an 'EventId' (or 'Nothing' if the event
  does not exist. -}
lookupEvent :: (Ord i) => EventId i -> VThread i d -> Maybe (Event i d)
lookupEvent (i,n) (VThread m) = case Map.lookup i m of
  -- Just (_,es) | length es > n -> Just $ es !! n
  Just (v,es) -> case lookupVC i v of
    Just n1 | n <= n1 && n >= d -> Just $ es !! (n - d)
      where d = (n1 + 1) - length es
    _ -> Nothing
  _ -> Nothing

{-| Get a single process's clock. -}
getClock :: (Ord i) => i -> VThread i d -> VClock i
getClock i t = fst $ getThread i t

meetClock :: (Ord i) => [i] -> VThread i d -> VClock i
meetClock [] _ = error "Can't get meetClock of []"
meetClock (i:[]) t = getClock i t
meetClock (i:is) t = meet (getClock i t) (meetClock is t)

{-| Get the observed-clock and event list for a given process ID.  If a
  process "does not exist" in the sense that it has never made an
  observation or produced an event, the result will be
  @('zeroClock',[])@. -}
getThread :: (Ord i) => i -> VThread i d -> (VClock i, [Event i d])
getThread i (VThread m) = case Map.lookup i m of
  Just (v,es) -> (v,es)
  Nothing -> (zeroClock, [])

{-| @observe i1 i2@ updates thread @i1@'s clock with information from
  @i2@.  When a process observes itself, nothing changes.

@
'observe' i i = 'id'
@
-}
observe :: (Ord i) => i -> i -> VThread i d -> VThread i d
observe i1 i2 (VThread m) = VThread $ Map.alter
  (\a ->
     let (v1,es) = case a of
                     Just (v,es) -> (v,es)
                     Nothing -> (zeroClock,[])
         v2 = fst $ getThread i2 (VThread m)
     in case joinVC v1 v2 of
          v' | v' /= zeroClock -> Just (v',es)
             | otherwise -> Nothing)
  i1
  m

{-| @'forkThread' old new@ initializes a thread with ID @new@ with no
  events and with a clock matching thread @old@.  If a thread with ID
  @new@ already exists (already has events), this returns 'Nothing'
  rather than erasing events. -}
forkThread :: (Ord i) => i -> i -> VThread i d -> Maybe (VThread i d)
forkThread old new t@(VThread m) =
  let (v,_) = getThread old t
      (_,es) = getThread new t
  in case es of
       [] -> Just . VThread $ Map.insert new (v,[]) m
       _ -> Nothing

{-| Remove all events not visible to the main clock of the given
  process. -}
reduceToVis :: (Ord i) => i -> VThread i d -> VThread i d
reduceToVis i t = reduceToVis' (fst $ getThread i t) t
-- reduceToVis i t@(VThread m) = VThread $
--   let (v,_) = getThread i t
--       f i1 (v1,es1) = case lookupVC i1 v of
--                         Just n -> Just (v1,take (n + 1) es1)
--                         Nothing -> Nothing
--   in Map.mapMaybeWithKey f m

reduceToVis' :: (Ord i) => VClock i -> VThread i d -> VThread i d
reduceToVis' v t@(VThread m) = VThread $
  let f i1 (v1,es1) = case lookupVC i1 v of
                        Just n -> Just (v1,take (n + 1) es1)
                        Nothing -> Nothing
  in Map.mapMaybeWithKey f m

{-| Turn a 'VThread' structure into a flat sequence of event payloads.
  Payloads are ordered based on causal order, with ties broken by
  comparing event-IDs (using 'simpleIdOrder'). -}
serialize :: (Ord i) => VThread i d -> [d]
serialize = serialize' simpleIdOrder

{-| A version of 'serialize' that takes a custom tie-breaking event-ID
  comparison.

@
'serialize'' 'simpleIdOrder' = 'serialize'
@
-}
serialize'
  :: (Ord i)
  => (EventId i -> EventId i -> Ordering)
  -> VThread i d
  -> [d]
serialize' tieBreak (VThread m) =
  map (snd.snd)
  . List.sortBy order
  . mconcat
  . map iddEvents
  . Map.assocs
  $ m
  where mkids i = zip (repeat i) [0..]
        iddEvents (i,(_,es)) = zip (mkids i) es
        order (i1,(v1,e1)) (i2,(v2,e2)) 
          | v1 `precedes` v2 = LT
          | v2 `precedes` v1 = GT
          | otherwise = tieBreak i1 i2

{-| Returns a pair @(t1,t2)@, where @t1@ contains the pruned events and
  @t2@ is the remaining (non-closed) 'VThread'. -}
prune :: (Ord i) => [i] -> VThread i d -> (VThread i d, VThread i d)
prune is t@(VThread m) = 
  let v = meetClock is t
      tDrop = reduceToVis' v t
      tKeep = VThread $ Map.mapWithKey f m
      f i (vi,es) = case lookupVC i v of
                      Just n -> (vi, drop (n + 1) es)
                      Nothing -> (vi,es)
  in (tDrop, tKeep)

{-| An ordering on event IDs that first compares sequence number (number
  of preceding events on the same process) and then process ID. -}
simpleIdOrder :: (Ord i) => EventId i -> EventId i -> Ordering
simpleIdOrder (i1,n1) (i2,n2) = case compare n1 n2 of
                                  EQ -> compare i1 i2
                                  o -> o

{-| Merge two 'VThread's.  This produces @'Left' i@ if header clocks
  indicate that process @i@ has diverged in the two versions. -}
mergeThread
  :: (Ord i)
  => VThread i d
  -> VThread i d
  -> Either i (VThread i d)
mergeThread (VThread m1) (VThread m2) = VThread <$> mergeA
  preserveMissing
  preserveMissing
  (zipWithAMatched $ \i a1 a2 -> case (a1,a2) of
     ((v1,_),(v2,_)) | v2 `precedes` v1 || v1 == v2 -> Right a1
                     | v1 `precedes` v2 -> Right a2
     _ -> Left i)
  m1
  m2

data UpdateClockError
  = MissingEvents
  | OldValues
  deriving (Show,Eq,Ord)

{-| Update a process's clock.  If the given clock precedes or is equal
  to the existing clock, @('Left' 'OldValues')@ is returned.  If the
  clock refers to events that are not yet present, @('Left'
  'MissingEvents')@ is returned.  If the given clock diverges from the
  existing clock, a runtime error arises. -}
updateClock
  :: (Ord i)
  => i
  -> VClock i
  -> VThread i d
  -> Either UpdateClockError (VThread i d)
updateClock i v t | not (v `leVC` totalClock t) = Left MissingEvents
updateClock i v (VThread m) =
  let m' = Map.alter
             (\a -> case a of
                      Just (v1,es) | v1 `precedes` v -> Just (v,es)
                                   | v `leVC` v1 -> Just (v1,es)
                                   | otherwise -> error $ 
                                       "updateClock: unrelated clock values"
                      Nothing | v /= zeroClock -> Just (v,[])
                              | otherwise -> Nothing)
            i m
  in if getClock i (VThread m) /= getClock i (VThread m')
        then Right (VThread m')
        else Left OldValues

{-| Get the clock witnessing all existing events. -}
totalClock :: (Ord i) => VThread i d -> VClock i
totalClock (VThread m) = Map.foldl' (\vt (v,_) -> vt `joinVC` v) zeroClock m

{-| Current number of events in a 'VThread'.  Note that this may be
  smaller than the number of ticks recorded in 'totalClock', due to
  pruning. -}
sizeVT :: (Ord i) => VThread i d -> Int
sizeVT (VThread m) = Map.foldl f 0 m
  where f n (_,es) = n + length es
