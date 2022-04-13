{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module UCap.Coord.StaticMap
  ( StaticMapG (..)
  , initStaticMapG
  ) where

import Data.InfMap
import UCap.Coord.Classes
import UCap.Domain.Classes
import UCap.Domain.StaticMap

import Data.Aeson
import Data.Bifunctor
import Data.Biapplicative
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Merge.Strict
import GHC.Generics

data StaticMapG k g
  = StaticMapG (Map k g)
  deriving (Generic)

deriving instance (Show k, Show g, Show (GId g)) => Show (StaticMapG k g)
deriving instance (Eq k, Eq g, Eq (GId g)) => Eq (StaticMapG k g)
deriving instance (Ord k, Ord g, Ord (GId g)) => Ord (StaticMapG k g)

instance (Ord k, Semigroup g) => Semigroup (StaticMapG k g) where
  StaticMapG a <> StaticMapG b = StaticMapG $ Map.unionWith (<>) a b

instance (ToJSON k, ToJSONKey k, ToJSON g) => ToJSON (StaticMapG k g) where
  toEncoding = genericToEncoding defaultOptions
instance (Ord k, FromJSONKey k, FromJSON k, FromJSON g) => FromJSON (StaticMapG k g)

localRead
  :: (Ord k, CoordSys g)
  => GId g -> StaticMapG k g -> GCap (StaticMapG k g)
localRead i (StaticMapG m) =
  let rs = Map.map (capsRead . localCaps i) m
  in StaticMapC $ rs

localWrite
  :: (Ord k, Ord (GState g), CoordSys g)
  => GId g -> StaticMapG k g -> GCap (StaticMapG k g)
localWrite i (StaticMapG m) = 
  let rs = Map.map (capsWrite . localCaps i) m
  in StaticMapC $ rs

unpackCaps :: (Ord k, Cap c) => Caps (StaticMapC k c) -> Map k (Caps c)
unpackCaps (Caps (StaticMapC rm) (StaticMapC wm)) = merge
  (mapMissing (\_ r -> Caps r idC))
  (mapMissing (\_ w -> Caps uniC w))
  (zipWithMatched (const Caps))
  rm
  wm

instance (Ord k, Ord (GState g), Eq (GEffect g), CoordSys g)
         => CoordSys (StaticMapG k g) where
  type GCap (StaticMapG k g) = StaticMapC k (GCap g)
  type GId (StaticMapG k g) = GId g

  localCaps i g = Caps (localRead i g) (localWrite i g)

  resolveEffect i (StaticMapE me) (StaticMapG mg) 
    | Map.isSubmapOfBy (\_ _ -> True) me mg =
      StaticMapG <$> Map.traverseWithKey
                 (\k g -> case Map.lookup k me of
                            Just e -> first (const idC)
                                            (resolveEffect i e g)
                            Nothing -> Right g)
                 mg
    | otherwise = Left idC

  resolveCaps i cs (StaticMapG gm) =
    let csm = unpackCaps cs
        m = merge
          (mapMissing (\_ _ -> DidFail Nothing))
          (mapMissing (\_ g -> WhenFail (Just g) idE))
          (zipWithMatched (\_ c g -> case resolveCaps i c g of
                                       Right e -> WhenFail (Just g) e
                                       Left yg -> DidFail yg))
          csm
          gm
    in failToEither $ bipure (fmap StaticMapG . traverse id) StaticMapE
                      <<*>> traverseBia id m
  grantRequests i (StaticMapG m) =
    fmap StaticMapG
    . traverse id
    . fmap (grantRequests i)
    $ m

initStaticMapG :: (Ord k) => [(k,g)] -> StaticMapG k g
initStaticMapG = StaticMapG . Map.fromList
