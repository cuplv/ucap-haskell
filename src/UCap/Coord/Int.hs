{-# LANGUAGE TypeFamilies #-}

module UCap.Coord.Int where

import UCap.Coord.Classes
import UCap.Domain.Classes
import UCap.Domain.Int

import Data.Bifunctor

data IncEscrow i
  = IncEscrow (EscrowIntPool i)
  deriving (Show,Eq,Ord)

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
