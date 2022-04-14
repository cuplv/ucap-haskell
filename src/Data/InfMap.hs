{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Data.InfMap
  ( InfMap (..)
  , uniform
  , fromList
  , fromMap
  , insert
  , adjust
  , revert
  , Data.InfMap.lookup
  , union
  , unionWith
  , unionWithA
  , Data.InfMap.map
  , toList
  , toMap
  , Data.InfMap.at
  , normalize
  ) where

import UCap.Domain.Classes

import Data.Aeson
import Data.Bifunctor
import Data.Biapplicative
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Merge.Strict
import GHC.Generics
import Lens.Micro.Platform

data InfMap k v =
  InfMap { baseVal :: v
         , otherVals :: Map k v
         }
  deriving (Show,Ord,Generic)

instance (Ord k, Eq v) => Eq (InfMap k v) where
  a == b = 
    let InfMap bv1 m1 = normalize a
        InfMap bv2 m2 = normalize b
    in bv1 == bv2 && m1 == m2

instance (ToJSON k, ToJSONKey k, ToJSON v) => ToJSON (InfMap k v)
instance (Ord k, FromJSON k, FromJSONKey k, FromJSON v) => FromJSON (InfMap k v)

instance (Ord k, Semigroup v) => Semigroup (InfMap k v) where
  (<>) = unionWith (<>)

instance (Ord k, Monoid v) => Monoid (InfMap k v) where
  mempty = uniform mempty

instance (Ord k, Meet v, Eq v) => Meet (InfMap k v) where
  meet = unionWith meet
  a <=? b = compareWith (<=?) (normalize a) (normalize b)

instance (Ord k, BMeet v, Eq v) => BMeet (InfMap k v) where
  meetId = uniform meetId

instance (Eq v, Ord k, Split v) => Split (InfMap k v) where
  split a b = bimap normalize normalize $ failToEither $ unionWithBia splitWF a b

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

unionWithBia
  :: (Ord k, Biapplicative p)
  => (a1 -> a2 -> p b1 b2)
  -> InfMap k a1
  -> InfMap k a2
  -> p (InfMap k b1) (InfMap k b2)
unionWithBia f (InfMap bv1 m1) (InfMap bv2 m2) =
  let ks = List.nub $ Map.keys m1 ++ Map.keys m2
      m = foldl (\a k -> Map.insert k k a) Map.empty ks
      tf k = case (Map.lookup k m1, Map.lookup k m2) of
               (Just v1, Just v2) -> f v1 v2
               (Just v1, Nothing) -> f v1 bv2
               (Nothing, Just v2) -> f bv1 v2
               _ -> error "Key is not in either map"
  in bipure InfMap InfMap
     <<*>> f bv1 bv2
     <<*>> traverseBia tf m

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
compareWith f m1 m2 = Data.InfMap.and (unionWith f m1 m2)

at :: (Ord k) => k -> Lens' (InfMap k v) v
at k = lens get set
  where get = Data.InfMap.lookup k
        set s b = insert k b s

{-| Delete any map entries with values equal to the base value.  This
  minimizes the representation size without changing the lookup
  values.

  'normalize' is applied by default as part of 'split', '==', and
  '<=?'.  It imposes an 'Eq' constraint on the value type @v@ that is
  a bit inconvenient, and so it is not automatically applied during
  'insert', 'union', etc.
-}
normalize :: (Eq v, Ord k) => InfMap k v -> InfMap k v
normalize (InfMap v0 m) = InfMap v0 $ Map.filter (/= v0) m
