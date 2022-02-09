{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module UCap.Coord.Const where

import UCap.Coord.Classes
import UCap.Domain.Classes
import UCap.Domain.Const

import Data.Bifunctor (first)

data ConstG g
  = ConstGToken (Token (GId g))
  | ConstGLower g

deriving instance (Show g, Show (GId g)) => Show (ConstG g)
deriving instance (Eq g, Eq (GId g)) => Eq (ConstG g)
deriving instance (Ord g, Ord (GId g)) => Ord (ConstG g)

instance (Ord (GId g), Ord (GState g), CoordSys g) => CoordSys (ConstG g) where
  type GCap (ConstG g) = ConstC' (GCap g)
  type GId (ConstG g) = GId g
  localCaps i (ConstGToken t) | i == tokenOwner t = fullCaps
                              | otherwise = emptyCaps
  localCaps i (ConstGLower g) =
    let cs = localCaps i g
    in Caps { capsRead = modifyC (capsRead cs)
            , capsWrite = modifyC (capsWrite cs)
            }
  resolveEffect i e g = case (e,g) of
    (e, ConstGToken t) | i == tokenOwner t -> Right g
                       | otherwise -> Left $ mincap e
    (ConstE s, ConstGLower g) -> Left (mincap $ ConstE s)
    (ModifyE e, ConstGLower g) -> case resolveEffect i e g of
       Right g -> Right $ ConstGLower g
       Left c -> Left $ modifyC c
  resolveCaps i cs g = case g of
    ConstGToken t | i == tokenOwner t -> Right ()
                  | cs `leqCaps` emptyCaps -> Right ()
                  | otherwise -> Left . Just . ConstGToken $ requestToken i t
    ConstGLower g -> case (capsRead cs, capsWrite cs) of
      (ConstC rs rc, ConstC ws wc) | ws == mempty -> fmap ConstGLower `first` resolveCaps i (Caps rc wc) g
                                   | otherwise -> Left Nothing
