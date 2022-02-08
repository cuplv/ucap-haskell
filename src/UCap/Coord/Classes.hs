{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module UCap.Coord.Classes where

import UCap.Domain.Classes

type family GEffect g where
  GEffect g = CEffect (GCap g)

type family GState g where
  GState g = CState (GCap g)

class (Cap (GCap g), Ord (GId g)) => CoordSys g where

  {-| The type of capabilities (which also defines the state and effect
  types). -}
  type GCap g

  {-| The type of replica IDs. -}
  type GId g

  {-| For given capability requirements, check if they are satisifed by
    the coordination system.  If so, return @'Right' ()@.  If not,
    attempt to perform requests that will eventually satisfy those
    requirements.  If this is possible, return the request-containing
    system in @'Left' ('Just' y)@.  If not, and thus there is no way
    to satisfy the requirements, return @'Left' 'Nothing'@. -}
  resolveCaps :: GId g -> Caps (GCap g) -> g -> Either (Maybe g) ()
  {-| For given effect, modify the coordination system to reflect its use.
    If the coordination system locally permits issuing the effect,
    then modify it accordingly and return @'Right' y@.  If not, return
    @'Left' c@, indicating a write failure with excess write
    capability @c@. -}
  resolveEffect :: GId g -> GEffect g -> g -> Either (GCap g) g
