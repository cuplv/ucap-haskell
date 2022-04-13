{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module UCap.Domain.PartMap where

import Data.InfMap (InfMap)
import qualified Data.InfMap as IM
import UCap.Domain.Classes
import UCap.Domain.Const
import UCap.Domain.Free

import Data.Aeson
import Data.Bifunctor
import Data.Biapplicative
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Merge.Strict
import GHC.Generics

data PartMapE p k e s
  = PartMapE (Map (p,k) (ConstE e s))
  deriving (Show, Eq, Ord, Generic)

instance (ToJSONKey p, ToJSON p, ToJSONKey k, ToJSON k, ToJSON e, ToJSON s)
  => ToJSON (PartMapE p k e s) where
  toEncoding = genericToEncoding defaultOptions
instance (Ord p, FromJSONKey p, FromJSON p, Ord k, FromJSONKey k, FromJSON k, FromJSON e, FromJSON s)
  => FromJSON (PartMapE p k e s)

type family PartMapE' p k e where
  PartMapE' p k e = PartMapE p k e (EDState e)

instance (Ord p, Ord k, EffectDom e, EDState e ~ s) => Semigroup (PartMapE p k e s) where
  PartMapE m1 <> PartMapE m2 = PartMapE $ Map.unionWith (<>) m1 m2

instance (Ord p, Ord k, EffectDom e, EDState e ~ s) => Monoid (PartMapE p k e s) where
  mempty = PartMapE Map.empty

instance (Ord p, Ord k, EffectDom e, EDState e ~ s) => EffectDom (PartMapE p k e s) where
  type EDState (PartMapE p k e s) = Map (p,k) s
  eFun (PartMapE me) ms = merge
    (mapMaybeMissing $ \_ e -> case e of
       ModifyE e -> Nothing
       ConstE s -> Just s)
    preserveMissing
    (zipWithMatched $ \_ e s -> case e of
       ModifyE e -> eFun e s
       ConstE s' -> s')
    me
    ms

modPme :: (p,k) -> e -> PartMapE p k e s
modPme k e = PartMapE $ Map.singleton k (ModifyE e)

insPme :: (p,k) -> s -> PartMapE p k e s
insPme k s = PartMapE $ Map.singleton k (ConstE s)

data PartMapC p k c
  = PartMapC (InfMap p FV) (InfMap (p,k) c)
  deriving (Show,Eq,Ord,Generic)

instance (ToJSON p, ToJSONKey p, ToJSON k, ToJSON c) => ToJSON (PartMapC p k c) where
  toEncoding = genericToEncoding defaultOptions
instance (Ord p, Ord k, FromJSON p, FromJSONKey p, FromJSON k, FromJSON c) => FromJSON (PartMapC p k c)

instance (Ord p, Ord k, Semigroup c) => Semigroup (PartMapC p k c) where
  PartMapC p1 m1 <> PartMapC p2 m2 = PartMapC (p1 <> p2) (m1 <> m2)

instance (Ord p, Ord k, Monoid c) => Monoid (PartMapC p k c) where
  mempty = PartMapC mempty mempty

instance (Ord p, Ord k, Meet c) => Meet (PartMapC p k c) where
  PartMapC p1 m1 `meet` PartMapC p2 m2 = PartMapC (meet p1 p2) (meet m1 m2)
  PartMapC p1 m1 <=? PartMapC p2 m2 = p1 <=? p2 && m1 <=? m2

instance (Ord p, Ord k, BMeet c) => BMeet (PartMapC p k c) where
  meetId = PartMapC meetId meetId

instance (Ord p, Ord k, Split c) => Split (PartMapC p k c) where
  PartMapC p1 m1 `split` PartMapC p2 m2 = failToEither $
    PartMapC <<$$>> splitWF p1 p2 <<*>> splitWF m1 m2

instance (Ord p, Ord k, Cap c) => Cap (PartMapC p k c) where
  type CEffect (PartMapC p k c) = PartMapE' p k (CEffect c)
  mincap (PartMapE m) = PartMapC
    (IM.fromMap idC $ Map.foldlWithKey
      (\a (p,_) e -> case e of
                       ConstE _ -> Map.insert p AnyChange a
                       _ -> a)
      Map.empty
      m)
    (IM.fromMap idC $ fmap
       (\e -> case e of
                ModifyE e' -> mincap e'
                _ -> idC)
       m)

capPmc :: (Ord p, Ord k, Cap c) => (p,k) -> c -> PartMapC p k c
capPmc k c = PartMapC mempty (IM.fromList idC [(k,c)])

insPmc :: (Ord p, Ord k, Cap c) => p -> PartMapC p k c
insPmc p = PartMapC (IM.fromList NoChange [(p,AnyChange)]) mempty

noInsPmc :: (Ord p, Ord k, Cap c) => p -> PartMapC p k c
noInsPmc p = PartMapC (IM.fromList AnyChange [(p,NoChange)]) meetId
