module Data.VClock
  ( VClock
  , zeroClock
  , tick
  , precedes
  , concurrent
  , lookupVC
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

{- | A vector clock using process ID type @i@. -}
data VClock i
  = VClock (Map i Int)
  deriving (Show,Eq,Ord)

instance (Ord i) => Semigroup (VClock i) where
  VClock m1 <> VClock m2 = VClock $ Map.unionWith max m1 m2

instance (Ord i) => Monoid (VClock i) where
  mempty = VClock Map.empty

{- | Initial clock, with all processes at zero.  This is a synonym for
   'Data.Monoid.mempty'. -}
zeroClock :: (Ord i) => VClock i
zeroClock = mempty

lookupVC :: (Ord i) => i -> VClock i -> Int
lookupVC i (VClock m) = case Map.lookup i m of
                          Just n -> n
                          Nothing -> 0

{- | Advance the clock for the given process ID. -}
tick :: (Ord i) => i -> VClock i -> VClock i
tick i (VClock m) = VClock $ Map.alter f i m
  where f (Just n) = Just (n + 1)
        f Nothing = Just 1

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
