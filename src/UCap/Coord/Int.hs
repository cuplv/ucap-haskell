{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module UCap.Coord.Int where

import UCap.Coord.Classes
import UCap.Domain.Classes
import UCap.Domain.Int

import Control.Applicative (liftA2)
import Data.Aeson
import Data.Bifunctor
import GHC.Generics

data IncEscrow i
  = IncEscrow (EscrowIntPool i)
  deriving (Show,Eq,Ord,Generic)

instance (Ord i, ToJSON i, ToJSONKey i) => ToJSON (IncEscrow i)
instance (Ord i, FromJSON i, FromJSONKey i) => FromJSON (IncEscrow i)

instance (Ord i) => Semigroup (IncEscrow i) where
  IncEscrow a <> IncEscrow b = IncEscrow (a <> b)

instance (Ord i) => CoordSys (IncEscrow i) where
  type GCap (IncEscrow i) = IncC
  type GId (IncEscrow i) = i
  resolveCaps i cs (IncEscrow g) =
    case incBound . capsWrite $ cs of
      Just wn | wn <= owned -> 
                case incBound . capsRead $ cs of
                  Just _ -> Right $ incE unowned
                  Nothing -> Right $ idE
              | otherwise -> 
                Left $ IncEscrow <$> escrowRequest' i (wn - owned) g
      Nothing -> Left Nothing
    where owned = escrowOwned i g
          unowned = escrowUnowned i g
  resolveEffect i e (IncEscrow g) = bimap incC IncEscrow $
    escrowUse i (intOffset e) g
  localCaps i (IncEscrow g) =
    incC <$> Caps { capsRead = escrowUnowned i g
                  , capsWrite = escrowOwned i g
                  }
  undoEffect i e (IncEscrow g) = IncEscrow $ escrowAdd i (intOffset e) g
  grantRequests i (IncEscrow g) = IncEscrow <$> escrowHandleReqs i g
  acceptGrants i (IncEscrow g) =
    let g' = escrowAccept i g
    in if g /= g'
          then Just $ IncEscrow g'
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
  deriving (Show,Eq,Ord,Generic)

instance (Ord i, ToJSON i, ToJSONKey i) => ToJSON (IntEscrow i)
instance (Ord i, FromJSON i, FromJSONKey i) => FromJSON (IntEscrow i)

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
