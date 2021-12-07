{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Data.UCap.Map where

import Data.UCap.Classes
import Data.UCap.Either
import Data.UCap.Identity

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics

type KeyE = EitherE (IdentityE ())

insertE :: (EffectDom e) => State e -> KeyE e
insertE = SetR

adjustE :: (EffectDom e) => e -> KeyE e
adjustE = OverLR idE

deleteE :: (EffectDom e) => KeyE e
deleteE = SetL ()

data MapE k e
  = MapE { emap :: Map k (KeyE e) }

instance (Ord k, EffectDom e) => Semigroup (MapE k e) where
  MapE a1 <> MapE a2 = MapE $ Map.unionWith (<>) a1 a2

instance (Ord k, EffectDom e) => Monoid (MapE k e) where
  mempty = MapE Map.empty

instance (Ord k, EffectDom e) => EffectDom (MapE k e) where
  type State (MapE k e) = Map k (State e)
  eFun (MapE es) s =
    let f k (SetR v) = Map.insert k v
        f k (SetL ()) = Map.delete k
        f k (OverLR IdentityC e) = Map.adjust (eFun e) k
    in Map.foldrWithKey' f s es

{-| Use a 'KeyE' effect to set, modify, or remove the value of a key in
  a map.

@
'eFun' ('mapE' k ('insertE' v)) = 'Data.Map.insert' k v
'eFun' ('mapE' k ('adjustE' e)) = 'Data.Map.adjust' k ('eFun' e)
'eFun' ('mapE' k 'deleteE') = 'Data.Map.delete' k
@
-}
mapE :: (Ord k, EffectDom e) => k -> KeyE e -> MapE k e
mapE k e = MapE $ Map.singleton k e

{-| Extract the 'KeyE' effect on a particular key that a 'MapE' effect
    contains.  If there is no effect on that key, this will return the
    identity effect 'Data.UCap.Classes.idE'.

@
'fromKeyE' k (mapE k ('adjustE' e) = 'adjustE' e
'fromKeyE' k1 (mapE k2 ('adjustE' e) = 'Data.UCap.Classes.idE'
@
-}
fromKeyE :: (Ord k, EffectDom e) => k -> MapE k e -> KeyE e
fromKeyE k (MapE m) = case Map.lookup k m of
  Just e -> e
  Nothing -> idE
