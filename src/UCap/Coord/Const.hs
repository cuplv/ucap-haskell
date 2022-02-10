{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module UCap.Coord.Const where

import UCap.Coord.Classes
import UCap.Domain.Classes
import UCap.Domain.Const

import Data.Bifunctor (bimap,first)

data ConstTG g = ConstTG (Token (GId g))

deriving instance (Show g, Show (GId g)) => Show (ConstTG g)
deriving instance (Eq g, Eq (GId g)) => Eq (ConstTG g)
deriving instance (Ord g, Ord (GId g)) => Ord (ConstTG g)

data ConstG g = ConstG g

deriving instance (Show g, Show (GId g)) => Show (ConstG g)
deriving instance (Eq g, Eq (GId g)) => Eq (ConstG g)
deriving instance (Ord g, Ord (GId g)) => Ord (ConstG g)

instance (Ord (GId g), Ord (GState g), CoordSys g) => CoordSys (ConstG g) where
  type GCap (ConstG g) = ConstC' (GCap g)
  type GId (ConstG g) = GId g
  localCaps i (ConstG g) =
    let cs = localCaps i g
    in Caps { capsRead = modifyC (capsRead cs)
            , capsWrite = modifyC (capsWrite cs)
            }
  resolveEffect i e (ConstG g) = case e of
    ModifyE e -> bimap modifyC ConstG $ resolveEffect i e g
    e -> Left . mincap $ e
  resolveCaps i cs (ConstG g) = case (capsRead cs, capsWrite cs) of
    (ConstC rs rc, ConstC ws wc) | ws == mempty ->
      let cs = Caps { capsRead = rc, capsWrite = wc }
      in bimap (fmap ConstG) ModifyE $ resolveCaps i cs g
                                 | otherwise -> Left Nothing

instance (Ord (GId g), Ord (GState g), CoordSys g) => CoordSys (ConstTG g) where
  type GCap (ConstTG g) = ConstC' (GCap g)
  type GId (ConstTG g) = GId g
  localCaps i (ConstTG t)
    | i == tokenOwner t = fullCaps
    | otherwise = emptyCaps
  resolveEffect i e g@(ConstTG t)
    | i == tokenOwner t = Right g
    | otherwise = Left . mincap $ e
  resolveCaps i cs (ConstTG t)
    | i == tokenOwner t = Right idE
    | cs `leqCaps` emptyCaps = Right idE
    | otherwise = Left . Just . ConstTG $ requestToken i t
