{-# LANGUAGE DeriveGeneric #-}

module UCap.Replica.VClock
  ( VClock
  , zeroClock
  , tick
  , precedes
  , concurrent
  , lookupVC
  , joinVC
  , changed
  , activeProcesses
  , toList
  ) where

import UCap.Domain.Classes

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Merge.Lazy
import GHC.Generics

{- | A vector clock using process ID type @i@. -}
data VClock i
  = VClock (Map i Int)
  deriving (Show,Eq,Ord,Generic)

instance (ToJSON i, ToJSONKey i) => ToJSON (VClock i)
instance (ToJSON i, ToJSONKey i) => ToJSONKey (VClock i)
instance (FromJSON i, FromJSONKey i, Ord i) => FromJSON (VClock i)
instance (FromJSON i, FromJSONKey i, Ord i) => FromJSONKey (VClock i)

instance (Ord i) => Meet (VClock i) where
  (<=?) = leVC
  meet (VClock m1) (VClock m2) = VClock $ merge
    dropMissing
    dropMissing
    (zipWithMatched $ const min)
    m1
    m2

joinVC :: (Ord i) => VClock i -> VClock i -> VClock i
joinVC (VClock m1) (VClock m2) = VClock $ merge
  preserveMissing
  preserveMissing
  (zipWithMatched $ const max)
  m1
  m2

{- | Initial clock, with no observed events for any process. -}
zeroClock :: (Ord i) => VClock i
zeroClock = VClock Map.empty

{-| Get the sequence number of the latest observed event for the given
  process ID (or 'Nothing' if no events have been observed
  for that process.  Sequence numbers start at @0@.

@
'lookupVC' i ('tick' i 'zeroClock') = 'Just' 0
@
 -}
lookupVC :: (Ord i) => i -> VClock i -> Maybe Int
lookupVC i (VClock m) = Map.lookup i m

{- | Advance the clock for the given process ID. -}
tick :: (Ord i) => i -> VClock i -> VClock i
tick i (VClock m) = VClock $ Map.alter f i m
  where f (Just n) = Just (n + 1)
        f Nothing = Just 0

leVC :: (Ord i) => VClock i -> VClock i -> Bool
leVC v1@(VClock m1) v2 = and . map f $ Map.keys m1
  where f k = lookupVC k v1 <= lookupVC k v2

{- | Check whether one clock denotes an event that happens before
   another.

@
v1 `'precedes'` v1 = 'False'

v1 `'precedes'` ('tick' i v1) = 'True'
@
-}
precedes :: (Ord i) => VClock i -> VClock i -> Bool
precedes v1 v2 = leVC v1 v2 && not (leVC v2 v1)

{- | Check whether two clocks denote concurrent events (or the same
   event). -}
concurrent :: (Ord i) => VClock i -> VClock i -> Bool
concurrent v1 v2 = not (precedes v1 v2) && not (precedes v2 v1)

{-| Get list of all process IDs with at least 1 event. -}
activeProcesses :: (Ord i) => VClock i -> [i]
activeProcesses (VClock m) = Map.keys m

{-| Get a list of process IDs that have updated between the first clock
  and the second.  The first clock is assumed to precede the second,
  so that all changes are positive.

@
changed v1 (tick "a" . tick "b" $ v1) = ["a","b"]
@
-}
changed :: (Ord i) => VClock i -> VClock i -> [i]
changed v1 v2 = filter
  (\i -> lookupVC i v1 < lookupVC i v2)
  (activeProcesses v2)

toList :: (Ord i) => VClock i -> [(i,Int)]
toList (VClock m) = Map.toList m
