{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.UCap.Map where

import Data.UCap.Classes
import Data.UCap.Const
import Data.UCap.Either
import Data.UCap.Identity
import Data.UCap.InfMap (InfMap)
import qualified Data.UCap.InfMap as IM
import Data.UCap.InfSet (InfSet)
import qualified Data.UCap.InfSet as IS
import Data.UCap.Lens

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics

type KeyE e = EitherE' (IdentityE ()) e

{-| Insert an entry into the map, or replace one that is already present.

@
'eFun' ('insertE' k v) = 'Map.insert' k v
@
-}
insertE :: (Ord k, EffectDom e) => k -> EDState e -> MapE k e
insertE k s = mapE k (SetR s)

{-| Adjust an entry in the map (if present).

@
'eFun' ('adjustE' k e) = 'Map.adjust' k ('eFun' e)
@
-}
adjustE :: (Ord k, EffectDom e) => k -> e -> MapE k e
adjustE k e = mapE k (OverLR idE e)

{-| Delete an entry from the map (if present).

@
'eFun' ('deleteE' k) = 'Map.delete' k
@
-}
deleteE :: (Ord k, EffectDom e) => k -> MapE k e
deleteE k = mapE k $ SetL ()

data MapE k e
  = MapE { emap :: Map k (KeyE e) }

instance (Ord k, EffectDom e) => Semigroup (MapE k e) where
  MapE a1 <> MapE a2 = MapE $ Map.unionWith (<>) a1 a2

instance (Ord k, EffectDom e) => Monoid (MapE k e) where
  mempty = MapE Map.empty

instance (Ord k, EffectDom e) => EffectDom (MapE k e) where
  type EDState (MapE k e) = Map k (EDState e)
  eFun (MapE es) s =
    let f k (SetR v) = Map.insert k v
        f k (SetL ()) = Map.delete k
        f k (OverLR IdentityC e) = Map.adjust (eFun e) k
    in Map.foldrWithKey' f s es

{-| Use a 'KeyE' effect to set, modify, or remove the value of a key in
  a map.

@
'eFun' ('mapE' k ('SetR' v)) = 'Data.Map.insert' k v
'eFun' ('mapE' k ('OverLR' 'idE' e)) = 'Data.Map.adjust' k ('eFun' e)
'eFun' ('mapE' k 'SetL ()') = 'Data.Map.delete' k
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
fromKeyE :: (Ord k, EffectDom e) => MapE k e -> k -> KeyE e
fromKeyE (MapE m) k = case Map.lookup k m of
  Just e -> e
  Nothing -> idE

type KeyC c s = EitherC (IdentityC ()) c () s

type KeyC' c = KeyC c (CState c)

insertAnyC :: (Monoid c) => KeyC' c
insertAnyC = setAnyR

-- adjustC :: KeyC' c
-- adjustC = onR

deleteC :: (Ord (CState c), Monoid c) => KeyC' c
deleteC = setAnyL

data MapC k c s
  = MapC { _unMapC :: InfMap k (KeyC c s) }
  deriving (Show,Eq,Ord,Generic)

type MapC' k c = MapC k c (CState c)

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
  , CState c ~ s
  , Eq (CEffect c)
  ) => Cap (MapC k c s) where
  type CEffect (MapC k c s) = MapE k (CEffect c)
  mincap (MapE m) = MapC $ IM.fromMap idC (Map.map mincap m)
  undo (MapE m) = MapC $ IM.fromMap idC (Map.map undo m)
  weaken (MapC m1) (MapC m2) = 
    case IM.toMap <$> IM.unionWithA weaken m1 m2 of
      Just (e,m) | e == idE -> Just (MapE m)
      _ -> Nothing

adjustC :: (Ord k, Cap c, Ord (CState c)) => k -> c -> MapC' k c
adjustC k c = MapC $ IM.fromList uniC [(k,onR c)]

onAnyC :: KeyC' c -> MapC' k c
onAnyC = MapC . IM.uniform

insertAny :: (Monoid c) => MapC' k c
insertAny = onAnyC insertAnyC

deleteAny :: (Ord (CState c), Monoid c) => MapC' k c
deleteAny = onAnyC deleteC

atMapC :: (Ord k) => k -> Lens' (MapC' k c) c
atMapC k = lens
  (\(MapC a) -> lowerC . overR . (^. IM.at k) $ a)
  (\(MapC a) c -> MapC $ a & IM.at k .~ onR c)
