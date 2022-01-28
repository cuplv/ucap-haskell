{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module UCap.Replica.VThread where

import UCap.Replica.Types
import UCap.Replica.VClock

import Control.Monad (foldM)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

data VThread i d
  -- Each thread holds a main clock and a list of events.  The main
  -- clock gives the highest clock number for every other thread that
  -- has been witnessed by that thread.
  = VThread (Map i (VClock i, [Event i d]))
  deriving (Show,Eq,Ord)

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

precedesE :: (Ord i) => VThread i d -> EventId i -> EventId i -> Bool
precedesE t i1 i2 = case (lookupEvent i1 t, lookupEvent i2 t) of
  (Just (v1,_), Just (v2,_)) ->
    tick (fst i1) v1 `precedes` tick (fst i2) v2
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

{-| Get an event referred to by an 'EventId' (or 'Nothing' if the event
  does not exist. -}
lookupEvent :: (Ord i) => EventId i -> VThread i d -> Maybe (Event i d)
lookupEvent (i,n) (VThread m) = case Map.lookup i m of
  Just (_,es) | length es >= n -> Just $ es !! n
  _ -> Nothing

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
reduceToVis i t@(VThread m) = VThread $
  let (v,_) = getThread i t
      f i1 (v1,es1) = case lookupVC i1 v of
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

{-| An ordering on event IDs that first compares sequence number (number
  of preceding events on the same process) and then process ID. -}
simpleIdOrder :: (Ord i) => EventId i -> EventId i -> Ordering
simpleIdOrder (i1,n1) (i2,n2) = case compare n1 n2 of
                                  EQ -> compare i1 i2
                                  o -> o
