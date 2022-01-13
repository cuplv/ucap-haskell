{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module UCap.Domain.Identity where

import UCap.Domain.Classes

import Data.Aeson
import GHC.Generics

data IdentityC s = IdentityC deriving (Show,Eq,Ord,Generic)

instance ToJSON (IdentityC s) where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON (IdentityC s)

instance Semigroup (IdentityC s) where
  IdentityC <> IdentityC = IdentityC

instance Monoid (IdentityC s) where
  mempty = IdentityC

instance Meet (IdentityC s) where
  meet IdentityC IdentityC = IdentityC
  IdentityC <=? IdentityC = True

instance BMeet (IdentityC s) where
  meetId = IdentityC

instance Split (IdentityC s)

instance EffectDom (IdentityC s) where
  type EDState (IdentityC s) = s
  eFun IdentityC = id

instance Cap (IdentityC s) where
  type CEffect (IdentityC s) = IdentityC s

type IdentityE = IdentityC
