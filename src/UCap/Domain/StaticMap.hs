{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module UCap.Domain.StaticMap where

import UCap.Domain.Classes


import Data.Aeson
import Data.Bifunctor
import Data.Biapplicative
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Merge.Strict
import GHC.Generics

data StaticMapE k e
  = StaticMapE (Map k e)
  deriving (Show, Eq, Ord, Generic)

type Sme = StaticMapE

sme = StaticMapE

instance (Ord k, Semigroup e) => Semigroup (StaticMapE k e) where
  StaticMapE a <> StaticMapE b = StaticMapE $ Map.unionWith (<>) a b

instance (Ord k, Monoid e) => Monoid (StaticMapE k e) where
  mempty = StaticMapE Map.empty

instance (Ord k, EffectDom e) => EffectDom (StaticMapE k e) where
  type EDState (StaticMapE k e) = Map k (EDState e)
  eFun (StaticMapE es) ss = merge
    (mapMissing (\_ _ -> error "Key missing from StaticMapE state."))
    (mapMissing (\_ s -> s))
    (zipWithMatched (\_ -> eFun))
    es
    ss

data StaticMapC k c
  = StaticMapC (Map k c)
  | StaticMapUni
  deriving (Show, Eq, Ord, Generic)

emptySmc :: StaticMapC k c
emptySmc = StaticMapC Map.empty

fromKeySmc :: (Ord k, Monoid c, BMeet c) => k -> StaticMapC k c -> c
fromKeySmc k (StaticMapC m) = case Map.lookup k m of
                                Just c -> c
                                Nothing -> idC
fromKeySmc _ StaticMapUni = uniC

toKeySmc :: (Meet c, Monoid c) => k -> c -> StaticMapC k c
toKeySmc k c | c <=? idC = emptySmc
             | otherwise = StaticMapC (Map.singleton k c)

type Smc = StaticMapC

smc = StaticMapC

smu = StaticMapUni

instance (Ord k, Semigroup c) => Semigroup (StaticMapC k c) where
  StaticMapC m1 <> StaticMapC m2 = StaticMapC $ merge
    preserveMissing
    preserveMissing
    (zipWithMatched (const (<>)))
    m1
    m2
  _ <> _ = StaticMapUni

instance (Ord k, Monoid c) => Monoid (StaticMapC k c) where
  mempty = emptySmc

instance (Ord k, Meet c, Monoid c, BMeet c) => Meet (StaticMapC k c) where
  StaticMapC m1 `meet` StaticMapC m2 = StaticMapC $ merge
    dropMissing
    dropMissing
    (zipWithMatched (const meet))
    m1
    m2
  a `meet` StaticMapUni = a
  StaticMapUni `meet` b = b

  StaticMapC m1 <=? b = Map.foldlWithKey
    (\a k c -> a && c <=? fromKeySmc k b)
    True
    m1
  StaticMapUni <=? StaticMapUni = True
  StaticMapUni <=? _ = False

instance (Ord k, Meet c, Monoid c, BMeet c) => BMeet (StaticMapC k c) where
  meetId = StaticMapUni

instance (Ord k, Monoid c, BMeet c, Split c) => Split (StaticMapC k c) where
  StaticMapUni `split` _ = Right StaticMapUni
  StaticMapC m1 `split` StaticMapC m2 =
    let m = merge 
              (mapMissing (\_ c -> split idC c))
              (mapMissing (\_ c -> Right c))
              (zipWithMatched (const split))
              m1
              m2
    in failToEither $ bipure StaticMapC StaticMapC 
                      <<*>> traverseBia (eitherToWF idC) m

instance (Ord k, Cap c) => Cap (StaticMapC k c) where
  type CEffect (StaticMapC k c) = StaticMapE k (CEffect c)
  mincap (StaticMapE m) = StaticMapC $ fmap mincap m
  undo (StaticMapE m) = StaticMapC $ fmap undo m
  weaken (StaticMapC m1) (StaticMapC m2) =
    let rs = merge
               (mapMissing (\_ c -> Just idE))
               (mapMissing (\_ c -> if c <=? idE
                                       then Just idE
                                       else Nothing))
               (zipWithMatched (const weaken))
               m1
               m2
    in StaticMapE <$> traverse id rs
