{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.UCap.Map where

import Data.UCap.Classes
import Data.UCap.Either
import Data.UCap.Identity
import Data.UCap.InfMap (InfMap)
import qualified Data.UCap.InfMap as IM
import Data.UCap.InfSet (InfSet)
import qualified Data.UCap.InfSet as IS

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics

type KeyE e = EitherE' (IdentityE ()) e

insertE :: (EffectDom e) => State e -> KeyE e
insertE = SetR

adjustE :: (EffectDom e) => e -> KeyE e
adjustE = OverLR idE

deleteE :: KeyE e
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

type KeyC c s = EitherC (IdentityC ()) c () s

type KeyC' c = KeyC c (State' c)

insertAnyC :: (Monoid c) => KeyC' c
insertAnyC = setAnyR

deleteC :: (Ord (State' c), Monoid c) => KeyC' c
deleteC = setAnyL

data MapC k c s
  = MapC (InfMap k (KeyC c s))
  deriving (Show,Eq,Ord,Generic)

type MapC' k c = MapC k c (State' c)

instance (Ord k, Ord s, Semigroup c) => Semigroup (MapC k c s) where
  MapC m1 <> MapC m2 = MapC (m1 <> m2)

instance (Ord k, Ord s, Monoid c) => Monoid (MapC k c s) where
  mempty = MapC mempty

instance (Ord k, Ord s, Meet c) => Meet (MapC k c s) where
  meet (MapC m1) (MapC m2) = MapC (meet m1 m2)

instance (Ord k, Ord s, BMeet c) => BMeet (MapC k c s) where
  meetId = MapC meetId

instance (Ord k, Ord s, Split c) => Split (MapC k c s) where
  split (MapC m1) (MapC m2) = MapC <$> split m1 m2

instance 
  ( Ord k
  , Ord s
  , Cap c
  , State' c ~ s
  , Eq (Effect c)
  ) => Cap (MapC k c s) where
  type Effect (MapC k c s) = MapE k (Effect c)
  mincap (MapE m) = MapC $ IM.fromMap idC (Map.map mincap m)
  undo (MapE m) = MapC $ IM.fromMap idC (Map.map undo m)
  weaken (MapC m1) (MapC m2) = 
    case IM.toMap <$> IM.unionWithA weaken m1 m2 of
      Just (e,m) | e == idE -> Just (MapE m)
      _ -> Nothing

onAnyC :: KeyC' c -> MapC' k c
onAnyC = MapC . IM.uniform

insertAny :: (Monoid c) => MapC' k c
insertAny = onAnyC insertAnyC

deleteAny :: (Ord (State' c), Monoid c) => MapC' k c
deleteAny = onAnyC deleteC
