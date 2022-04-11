{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module UCap.Coord.Map where

import Data.InfMap
import UCap.Coord.Classes
import UCap.Domain.Classes
import UCap.Domain.Either
import UCap.Domain.Identity
import UCap.Domain.Map

import Data.Bifunctor
import Data.Biapplicative
import Data.Map (Map)
import qualified Data.Map as Map

data MapG k g = MapG (Map k g)

deriving instance (Show k, Show g, Show (GId g)) => Show (MapG k g)
deriving instance (Eq k, Eq g, Eq (GId g)) => Eq (MapG k g)
deriving instance (Ord k, Ord g, Ord (GId g)) => Ord (MapG k g)

instance (Ord k, Semigroup g) => Semigroup (MapG k g) where
  MapG a <> MapG b = MapG $ Map.unionWith (<>) a b

localRead
  :: (Ord k, Ord (GState g), CoordSys g)
  => GId g -> MapG k g -> GCap (MapG k g)
localRead i (MapG m) = 
  let rs = Map.map (capsRead . localCaps i) m
  in MapC . InfMap idC . Map.map onR $ rs

localWrite
  :: (Ord k, Ord (GState g), CoordSys g)
  => GId g -> MapG k g -> GCap (MapG k g)
localWrite i (MapG m) = 
  let rs = Map.map (capsWrite . localCaps i) m
  in MapC . InfMap idC . Map.map onR $ rs

singleEff
  :: (CoordSys g)
  => GId g
  -> EitherE' (IdentityE ()) (GEffect g)
  -> g
  -> Either (GCap g) g
singleEff i (OverLR _ e) g = resolveEffect i e g
singleEff i e g = error "Insert/Delete not supported"


{-| Unpack 'Caps' over 'MapC'', giving the 'Caps' that covers unlisted keys and map of 'Caps' for explicit keys. -}
unpackMCaps :: Caps (MapC' k c) -> (Caps c, Map k (Caps c))
unpackMCaps (Caps (MapC (InfMap ru rm)) (MapC (InfMap wu wm))) = 
  let u = Caps undefined
  in undefined

instance (Ord k, Ord (GState g), Eq (GEffect g), CoordSys g)
         => CoordSys (MapG k g) where
  type GCap (MapG k g) = MapC' k (GCap g)
  type GId (MapG k g) = GId g

  localCaps i g = Caps (localRead i g) (localWrite i g)

  resolveEffect i (MapE me) (MapG mg) | Map.isSubmapOfBy (\_ _ -> True) me mg =
    MapG <$> Map.traverseWithKey
               (\k g -> case Map.lookup k me of
                          Just e -> first (const idC) (singleEff i e g)
                          Nothing -> Right g)
               mg
  resolveEffect _ _ _ = Left idC

  resolveCaps i (Caps rm wm) (MapG m) =
    let csm = undefined
    in undefined
  -- grantRequests i (MapG m) = _
