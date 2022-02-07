{-# LANGUAGE DeriveGeneric #-}

module Data.InfNum
  ( AddBound
  , addB
  , addFun
  , MulBound
  , mulB
  , mulFun
  , addId
  , mulId
  ) where

import UCap.Domain.Classes

import Data.Aeson
import GHC.Generics

addId :: (Num n) => n
addId = 0

mulId :: (Num n) => n
mulId = 1

data AddBound n
  = AddBounded n
  | AddInfinite
  deriving (Eq,Ord,Generic)

instance (Show n) => Show (AddBound n) where
  show (AddBounded n) = show n
  show AddInfinite = "∞"

instance (FromJSON n) => FromJSON (AddBound n)
instance (FromJSON n, FromJSONKey n) => FromJSONKey (AddBound n)
instance (ToJSON n) => ToJSON (AddBound n) where
  toEncoding = genericToEncoding defaultOptions
instance (ToJSON n, ToJSONKey n) => ToJSONKey (AddBound n)

addB :: (Num n, Ord n) => n -> AddBound n
addB n | n >= addId = AddBounded n
       | otherwise = error "Can't construct add bound < 0"

addFun :: (Num n) => AddBound n -> Maybe n
addFun (AddBounded n) = Just n
addFun AddInfinite = Nothing

instance (Num n) => Semigroup (AddBound n) where
  AddBounded n1 <> AddBounded n2 = AddBounded (n1 + n2)
  _ <> AddInfinite = AddInfinite
  AddInfinite <> _ = AddInfinite

instance (Num n) => Monoid (AddBound n) where
  mempty = AddBounded addId

instance (Ord n) => Meet (AddBound n) where
  AddBounded n1 `meet` AddBounded n2 = AddBounded (min n1 n2)
  b1 `meet` AddInfinite = b1
  AddInfinite `meet` b2 = b2

  AddBounded n1 <=? AddBounded n2 = n1 <= n2
  b1 <=? AddInfinite = True
  AddInfinite <=? AddBounded _ = False

instance (Ord n) => BMeet (AddBound n) where
  meetId = AddInfinite

instance (Num n, Ord n) => Split (AddBound n) where
  split (AddBounded n1) (AddBounded n2)
    | n1 >= n2 = Right $ AddBounded (n1 - n2)
    | otherwise = Left $ AddBounded (n2 - n1)
  split (AddBounded _) AddInfinite = Left AddInfinite
  split AddInfinite _ = Right AddInfinite

data MulBound n
  = MulBounded n
  | MulInfinite
  deriving (Eq,Ord,Generic)

instance (Show n) => Show (MulBound n) where
  show (MulBounded n) = show n
  show MulInfinite = "∞"

instance (FromJSON n) => FromJSON (MulBound n)
instance (FromJSON n, FromJSONKey n) => FromJSONKey (MulBound n)
instance (ToJSON n) => ToJSON (MulBound n)
instance (ToJSON n, ToJSONKey n) => ToJSONKey (MulBound n)

mulB :: (Integral n, Ord n) => n -> MulBound n
mulB n | n >= mulId = MulBounded n
       | otherwise = error "Can't construct mul bound < 1"

mulFun :: (Integral n) => MulBound n -> Maybe n
mulFun (MulBounded n) = Just n
mulFun MulInfinite = Nothing

instance (Integral n) => Semigroup (MulBound n) where
  MulBounded n1 <> MulBounded n2 = MulBounded (n1 * n2)
  _ <> MulInfinite = MulInfinite
  MulInfinite <> _ = MulInfinite

instance (Integral n) => Monoid (MulBound n) where
  mempty = MulBounded mulId

instance (Ord n) => Meet (MulBound n) where
  MulBounded n1 `meet` MulBounded n2 = MulBounded (min n1 n2)
  b1 `meet` MulInfinite = b1
  MulInfinite `meet` b2 = b2

  MulBounded n1 <=? MulBounded n2 = n1 <= n2
  b1 <=? MulInfinite = True
  MulInfinite <=? MulBounded _ = False

instance (Ord n) => BMeet (MulBound n) where
  meetId = MulInfinite

instance (Integral n, Ord n) => Split (MulBound n) where
  split (MulBounded n1) (MulBounded n2)
    | n1 >= n2 = Right . MulBounded $ n1 `div` n2
    | otherwise = Left . MulBounded $ n2 `div` n1
  split (MulBounded _) MulInfinite = Left MulInfinite
  split MulInfinite _ = Right MulInfinite
