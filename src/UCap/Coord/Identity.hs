{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module UCap.Coord.Identity
  (IdentityG (..)
  ) where

import UCap.Coord.Classes
import UCap.Domain.Classes
import UCap.Domain.Identity

{-| The 'IdentityG' coordination system is for static, read-only data.
  It fixes all replicas with empty local capabilities, thus giving
  them all full read capabilities.

@
'resolveEffect' i ('addE' 5) 'IdentityG' = 'Left' ('addC' 5)

'resolveEffect' i 'idE' 'IdentityG' = 'Right' ()
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
       then Right ()
       else Left Nothing
  resolveEffect _ e IdentityG =
    if e == idE
       then Right IdentityG
       else Left (mincap e)
  localCaps _ IdentityG = Caps { capsRead = uniC
                               , capsWrite = idC
                               }
