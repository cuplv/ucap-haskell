{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module UCap.Domain.Free where

import UCap.Domain.Classes

import Data.Bifunctor
import Data.Aeson
import GHC.Generics

data FreeE s
  = FreeSet s
  | FreeId
  deriving (Show,Eq,Ord,Generic)

instance (ToJSON s) => ToJSON (FreeE s) where
  toEncoding = genericToEncoding defaultOptions
instance (FromJSON s) => FromJSON (FreeE s)

instance Semigroup (FreeE s) where
  FreeId <> a = a
  a <> FreeId = a
  _ <> a = a

instance Monoid (FreeE s) where
  mempty = FreeId

instance EffectDom (FreeE s) where
  type EDState (FreeE s) = s
  eFun (FreeSet s) = const s
  eFun FreeId = id

data FV
  = NoChange
  | AnyChange
  deriving (Show,Eq,Ord,Generic)

instance ToJSON FV where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON FV

instance Semigroup FV where
  NoChange <> NoChange = NoChange
  _ <> AnyChange = AnyChange
  AnyChange <> _ = AnyChange

instance Monoid FV where
  mempty = NoChange

instance Meet FV where
  NoChange `meet` _ = NoChange
  _ `meet` NoChange = NoChange
  AnyChange `meet` AnyChange = AnyChange

  AnyChange <=? NoChange = False
  _ <=? _ = True

instance BMeet FV where
  meetId = AnyChange

instance Split FV where
  NoChange `split` AnyChange = Left AnyChange
  a `split` b = Right a

data FreeC e
  = FreeC { freeC :: FV }
  deriving (Show,Eq,Ord,Generic)

instance ToJSON (FreeC e) where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON (FreeC e)

instance Semigroup (FreeC e) where
  c1 <> c2 = FreeC $ freeC c1 <> freeC c2

instance Monoid (FreeC e) where
  mempty = FreeC mempty

instance Meet (FreeC e) where
  c1 `meet` c2 = FreeC $ freeC c1 `meet` freeC c2

  c1 <=? c2 = freeC c1 <=? freeC c2

instance BMeet (FreeC e) where
  meetId = FreeC meetId

instance Split (FreeC e) where
  FreeC a `split` FreeC b = bimap FreeC FreeC $ split a b

instance (EffectDom e, Eq e) => Cap (FreeC e) where
  type CEffect (FreeC e) = e
