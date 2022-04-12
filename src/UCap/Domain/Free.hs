{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module UCap.Domain.Free where

import UCap.Domain.Classes

import Data.Aeson
import GHC.Generics

data FreeC e
  = NoChange
  | AnyChange
  deriving (Show,Eq,Ord,Generic)

instance ToJSON (FreeC e) where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON (FreeC e)

instance Semigroup (FreeC e) where
  NoChange <> NoChange = NoChange
  _ <> AnyChange = AnyChange
  AnyChange <> _ = AnyChange

instance Monoid (FreeC e) where
  mempty = NoChange

instance Meet (FreeC e) where
  NoChange `meet` _ = NoChange
  _ `meet` NoChange = NoChange
  AnyChange `meet` AnyChange = AnyChange

  AnyChange <=? NoChange = False
  _ <=? _ = True

instance BMeet (FreeC e) where
  meetId = AnyChange

instance Split (FreeC e)

instance (EffectDom e, Eq e) => Cap (FreeC e) where
  type CEffect (FreeC e) = e
