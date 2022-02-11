{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module UCap.Coord.Const where

import UCap.Coord.Classes
import UCap.Domain.Classes
import UCap.Domain.Const

import Data.Bifunctor (bimap,first)

data ConstG g = ConstG g

deriving instance (Show g, Show (GId g)) => Show (ConstG g)
deriving instance (Eq g, Eq (GId g)) => Eq (ConstG g)
deriving instance (Ord g, Ord (GId g)) => Ord (ConstG g)

instance (Semigroup g) => Semigroup (ConstG g) where
  ConstG a <> ConstG b = ConstG (a <> b)

fullConstG :: (Ord (CState c), Cap c) => Caps (ConstC' c)
fullConstG = Caps { capsRead = idC
                  , capsWrite = modifyC uniC
                  }

instance (Ord (GId g), Ord (GState g), CoordSys g)
         => CoordSys (ConstG g) where
  type GCap (ConstG g) = ConstC' (GCap g)
  type GId (ConstG g) = GId g
  localCaps i (ConstG g) = modifyC <$> localCaps i g
  resolveEffect i e (ConstG g) = case e of
    ModifyE e -> bimap modifyC ConstG $ resolveEffect i e g
    e -> Left . mincap $ e
  resolveCaps i cs (ConstG g)
    | cs `leqCaps` fullConstG =
      bimap (fmap ConstG) ModifyE $ resolveCaps i (lowerC <$> cs) g
    | otherwise = Left Nothing
