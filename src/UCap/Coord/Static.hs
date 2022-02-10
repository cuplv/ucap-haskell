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

{-| The 'IdentityG' coordination system is for static, read-only data.
  It fixes all replicas with empty local capabilities, thus giving
  them all full read capabilities.

@
'resolveEffect' i ('addE' 5) 'IdentityG' = 'Left' ('addC' 5)

'resolveEffect' i 'idE' 'IdentityG' = 'Right' 'id'
@
-}
data IdentityG i c = IdentityG deriving (Show,Eq,Ord)

instance Semigroup (IdentityG i c) where
  g1 <> _ = g1

instance Monoid (IdentityG i c) where
  mempty = IdentityG

instance (Cap c, Eq (CEffect c)) => CoordSys (IdentityG i c) where
  type GId (IdentityG i c) = i
  type GCap (IdentityG i c) = c
  resolveCaps _ (Caps _ wc) IdentityG =
    if isId wc
       then Right id
       else Left Nothing
  resolveEffect _ e IdentityG =
    if e == idE
       then Right IdentityG
       else Left (mincap e)
  localCaps _ IdentityG = Caps { capsRead = idC
                               , capsWrite = idC
                               }

data UniversalG i c = UniversalG deriving (Show,Eq,Ord)

instance Semigroup (UniversalG i c) where
  g1 <> _ = g1
  
instance Monoid (UniversalG i c) where
  mempty = UniversalG

instance (Cap c, Eq (CEffect c)) => CoordSys (UniversalG i c) where
  type GId (UniversalG i c) = i
  type GCap (UniversalG i c) = c
  resolveCaps _ (Caps rc _) UniversalG | isUni rc = Right id
                                       | otherwise = Left Nothing
  resolveEffect _ e UniversalG = Right UniversalG
  localCaps _ UniversalG = Caps { capsRead = uniC
                                , capsWrite = uniC
                                }
