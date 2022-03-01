{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module UCap.Coord.Int
  ( IntEscrow
  , initIntEscrow
  ) where

import UCap.Coord.Classes
import UCap.Domain.Classes
import UCap.Domain.Int

import Control.Applicative (liftA2)
import Data.Aeson
import Data.Bifunctor
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics

data IncEscrow i
  = IncEscrow { ieBufferFactor :: Int
              , iePool :: EscrowIntPool i
              }
  deriving (Show,Eq,Ord,Generic)

instance (Ord i, ToJSON i, ToJSONKey i) => ToJSON (IncEscrow i)
instance (Ord i, FromJSON i, FromJSONKey i) => FromJSON (IncEscrow i)

instance (Ord i) => Semigroup (IncEscrow i) where
  IncEscrow bf a <> IncEscrow _ b = IncEscrow bf (a <> b)

instance (Ord i) => CoordSys (IncEscrow i) where
  type GCap (IncEscrow i) = IncC
  type GId (IncEscrow i) = i
  resolveCaps i cs (IncEscrow bf g) =
    case incBound . capsWrite $ cs of
      Just wn | wn <= owned -> 
                case incBound . capsRead $ cs of
                  Just _ -> Right $ incE unowned
                  Nothing -> Right $ idE
              | otherwise ->
                let amt = (wn - owned) * bf -- overrequest by buffer-factor
                in Left $ IncEscrow bf <$> escrowRequest' i amt g
      Nothing -> Left Nothing
    where owned = escrowOwned i g
          unowned = escrowUnowned i g
  resolveEffect i e (IncEscrow bf g) = bimap incC (IncEscrow bf) $
    escrowUse i (intOffset e) g
  localCaps i (IncEscrow _ g) =
    incC <$> Caps { capsRead = escrowUnowned i g
                  , capsWrite = escrowOwned i g
                  }
  undoEffect i e (IncEscrow bf g) =
    IncEscrow bf $ escrowAdd i (intOffset e) g
  grantRequests i (IncEscrow bf g) = IncEscrow bf <$> escrowHandleReqs i g
  acceptGrants i (IncEscrow bf g) =
    let g' = escrowAccept i g
    in if g /= g'
          then Just $ IncEscrow bf g'
          else Nothing

data DecEscrow i
  = DecEscrow (IncEscrow i)
  deriving (Show,Eq,Ord,Generic)

instance (Ord i, ToJSON i, ToJSONKey i) => ToJSON (DecEscrow i)
instance (Ord i, FromJSON i, FromJSONKey i) => FromJSON (DecEscrow i)

instance (Ord i) => Semigroup (DecEscrow i) where
  DecEscrow a <> DecEscrow b = DecEscrow (a <> b)

instance (Ord i) => CoordSys (DecEscrow i) where
  type GCap (DecEscrow i) = DecC
  type GId (DecEscrow i) = i
  resolveCaps i cs (DecEscrow g) = bimap (fmap DecEscrow) DecE $
    resolveCaps i (unwrapDecC <$> cs) g
  resolveEffect i (DecE e) (DecEscrow g) = bimap DecC DecEscrow $
    resolveEffect i e g
  localCaps i (DecEscrow g) = DecC <$> localCaps i g
  undoEffect i (DecE e) (DecEscrow g) = DecEscrow $ undoEffect i e g
  grantRequests i (DecEscrow g) = DecEscrow <$> grantRequests i g
  acceptGrants i (DecEscrow g) = DecEscrow <$> acceptGrants i g

data IntEscrow i
  = IntEscrow { addEscrow :: IncEscrow i
              , subEscrow :: DecEscrow i
              }
  deriving (Eq,Ord,Generic)

instance (Ord i, ToJSON i, ToJSONKey i) => ToJSON (IntEscrow i)
instance (Ord i, FromJSON i, FromJSONKey i) => FromJSON (IntEscrow i)

instance (Ord i, Show i) => Show (IntEscrow i) where
  show e@(IntEscrow (IncEscrow _ a) (DecEscrow (IncEscrow _ s))) =
    let ks = nub $ escrowOwners a ++ escrowOwners s
        caps = map (\k -> (k,capsWrite $ localCaps k e)) ks
    in show caps

instance (Ord i) => Semigroup (IntEscrow i) where
  IntEscrow a1 s1 <> IntEscrow a2 s2 = IntEscrow (a1 <> a2) (s1 <> s2)

instance (Ord i) => CoordSys (IntEscrow i) where
  type GCap (IntEscrow i) = IntC
  type GId (IntEscrow i) = i
  resolveCaps i cs (IntEscrow a s) =
    let ar = eitherToWF (pure a) $ AddE <$> resolveCaps i (addBnd <$> cs) a
        sr = eitherToWF (pure s) $ SubE <$> resolveCaps i (subBnd <$> cs) s
        f = WhenFail (liftA2 IntEscrow) (<>)
    in failToEither $ f <<*>> ar <<*>> sr
  resolveEffect i e (IntEscrow a s) = case e of
    AddE e -> bimap
      fromIncC
      (\a -> IntEscrow a (undoEffect i (DecE e) s))
      (resolveEffect i e a)
    SubE (DecE e) -> bimap
      fromDecC
      (\s -> IntEscrow (undoEffect i e a) s)
      (resolveEffect i (DecE e) s)
  localCaps i (IntEscrow a s) = IntC <$> localCaps i a <*> localCaps i s
  undoEffect i e (IntEscrow a s) = case e of
    AddE e -> IntEscrow (undoEffect i e a) s
    SubE e -> IntEscrow a (undoEffect i e s)
  grantRequests i (IntEscrow a s) =
    l2j . failToEither $ WhenFail IntEscrow (,)
      <<*>> (eitherToWF a . j2l $ grantRequests i a)
      <<*>> (eitherToWF s . j2l $ grantRequests i s)
  acceptGrants i (IntEscrow a s) =
    l2j . failToEither $ WhenFail IntEscrow (,)
      <<*>> (eitherToWF a . j2l $ acceptGrants i a)
      <<*>> (eitherToWF s . j2l $ acceptGrants i s)


{-| Create the initial state of an 'IntEscrow' coordination system.

  The buffer factor controls the size of requests.  With a factor of
  @1@, requests are made for only what is immediately needed.  A
  factor of @3@ triples request size, covering three transactions in
  one request.  The buffer factor must be @>= 1@.

  Resources are always requested from a "source peer".  Currently,
  they are only requested from the first source peer given.  If no
  source peers are given, resources cannot be requested.
-}
initIntEscrow
  :: (Ord i)
  => Int -- ^ Buffer factor
  -> [i] -- ^ Source peers
  -> Map i (Int,Int) -- ^ Initial holdings
  -> IntEscrow i
initIntEscrow 0 _ _ = error "Buffer factor is 0, must be >= 1"
initIntEscrow bf sources m = IntEscrow
  { addEscrow = IncEscrow bf $
                initEscrow sources [] (Map.map snd m)
  , subEscrow = DecEscrow . IncEscrow bf $
                initEscrow sources [] (Map.map fst m)
  }
