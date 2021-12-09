{-# LANGUAGE DeriveGeneric #-}

module Data.UCap.InfMap
  ( InfMap
  , uniform
  , fromList
  , fromMap
  , insert
  , adjust
  , revert
  , Data.UCap.InfMap.lookup
  , union
  , unionWith
  , unionWithA
  , Data.UCap.InfMap.map
  , toList
  , toMap
  ) where

import Data.UCap.Classes

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Merge.Strict
import GHC.Generics

data InfMap k v =
  InfMap { baseVal :: v
         , otherVals :: Map k v
         }
  deriving (Show,Eq,Ord,Generic)

instance (Ord k, Semigroup v) => Semigroup (InfMap k v) where
  (<>) = unionWith (<>)

instance (Ord k, Monoid v) => Monoid (InfMap k v) where
  mempty = uniform mempty

instance (Ord k, Meet v) => Meet (InfMap k v) where
  meet = unionWith meet
  (<=?) = compareWith (<=?)

instance (Ord k, BMeet v) => BMeet (InfMap k v) where
  meetId = uniform meetId

instance (Ord k, Split v) => Split (InfMap k v) where
  split = unionWithA split

uniform :: v -> InfMap k v
uniform v = InfMap v Map.empty

fromList :: (Ord k) => v -> [(k,v)] -> InfMap k v
fromList v vs = InfMap v (Map.fromList vs)

toMap :: InfMap k v -> (v, Map k v)
toMap (InfMap v0 m) = (v0,m)

toList :: InfMap k v -> (v,[(k,v)])
toList (InfMap v0 m) = (v0, Map.toList m)

fromMap :: v -> Map k v -> InfMap k v
fromMap = InfMap

lookup :: (Ord k) => k -> InfMap k v -> v
lookup k (InfMap v0 m) = Map.findWithDefault v0 k m

insert :: (Ord k) => k -> v -> InfMap k v -> InfMap k v
insert k v (InfMap v0 m) = InfMap v0 (Map.insert k v m)

adjust :: (Ord k) => (v -> v) -> k -> InfMap k v -> InfMap k v
adjust f k (InfMap v0 m) = InfMap v0 (Map.alter f' k m)
  where f' (Just v) = Just (f v)
        f' Nothing = Just (f v0)

revert :: (Ord k) => k -> InfMap k v -> InfMap k v
revert k (InfMap v0 m) = InfMap v0 (Map.delete k m)

map :: (a -> b) -> InfMap k a -> InfMap k b
map f (InfMap v0 m) = InfMap (f v0) (Map.map f m)

unionWith
  :: (Ord k)
  => (a -> b -> c)
  -> InfMap k a
  -> InfMap k b
  -> InfMap k c
unionWith f (InfMap v01 m1) (InfMap v02 m2) =
  let m = merge
            (mapMissing (\_ v1 -> f v1 v02))
            (mapMissing (\_ v2 -> f v01 v2))
            (zipWithMatched (\_ -> f))
            m1
            m2
  in InfMap (f v01 v02) m

unionWithA
  :: (Ord k, Applicative m)
  => (a -> b -> m c)
  -> InfMap k a
  -> InfMap k b
  -> m (InfMap k c)
unionWithA f (InfMap v01 m1) (InfMap v02 m2) =
  let m = mergeA
            (traverseMissing (\_ v1 -> f v1 v02))
            (traverseMissing (\_ v2 -> f v01 v2))
            (zipWithAMatched (\_ -> f))
            m1
            m2
  in InfMap <$> f v01 v02 <*> m

union :: (Ord k) => InfMap k v -> InfMap k v -> InfMap k v
union = unionWith (\a _ -> a)

and :: InfMap k Bool -> Bool
and (InfMap v0 m) = Prelude.and $ v0 : Map.elems m

or :: InfMap k Bool -> Bool
or (InfMap v0 m) = Prelude.or $ v0 : Map.elems m

compareWith
  :: (Ord k)
  => (v -> v -> Bool)
  -> InfMap k v
  -> InfMap k v
  -> Bool
compareWith f m1 m2 = Data.UCap.InfMap.and (unionWith f m1 m2)
