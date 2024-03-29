{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module UCap.Coord.Static
  ( IdentityG (..)
  , UniversalG (..)
  ) where

import UCap.Coord.Classes
import UCap.Domain.Classes
import UCap.Domain.Identity

import Data.Aeson
import GHC.Generics

{-| The 'IdentityG' coordination system is for static, read-only data.
  It fixes all replicas with empty local capabilities, thus giving
  them all full read capabilities.

@
'resolveEffect' i ('addE' 5) 'IdentityG' = 'Left' ('addC' 5)

'resolveEffect' i 'idE' 'IdentityG' = 'Right' 'id'
@
-}
data IdentityG i c = IdentityG deriving (Show,Eq,Ord,Generic)

instance ToJSON (IdentityG i c)
instance FromJSON (IdentityG i c)

instance Semigroup (IdentityG i c) where
  g1 <> _ = g1

instance Monoid (IdentityG i c) where
  mempty = IdentityG

instance (Ord i, Cap c, Eq (CEffect c)) => CoordSys (IdentityG i c) where
  type GId (IdentityG i c) = i
  type GCap (IdentityG i c) = c
  resolveCaps _ (Caps _ wc) IdentityG =
    if isId wc
       then Right idE
       else Left Nothing
  resolveEffect _ e IdentityG =
    if e == idE
       then Right IdentityG
       else Left (mincap e)
  localCaps _ IdentityG = Caps { capsRead = idC
                               , capsWrite = idC
                               }

data UniversalG i c = UniversalG deriving (Show,Eq,Ord,Generic)

instance ToJSON (UniversalG i c)
instance FromJSON (UniversalG i c)

instance Semigroup (UniversalG i c) where
  g1 <> _ = g1
  
instance Monoid (UniversalG i c) where
  mempty = UniversalG

instance (Ord i, Cap c, Eq (CEffect c)) => CoordSys (UniversalG i c) where
  type GId (UniversalG i c) = i
  type GCap (UniversalG i c) = c
  resolveCaps _ (Caps rc _) UniversalG | isUni rc = Right idE
                                       | otherwise = Left Nothing
  resolveEffect _ e UniversalG = Right UniversalG
  localCaps _ UniversalG = Caps { capsRead = uniC
                                , capsWrite = uniC
                                }
