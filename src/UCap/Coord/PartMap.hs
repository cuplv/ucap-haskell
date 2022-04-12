{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module UCap.Coord.PartMap where

import Data.InfMap (InfMap)
import qualified Data.InfMap as IM
import UCap.Coord.Classes
import UCap.Coord.StaticMap
import UCap.Domain.Classes
import UCap.Domain.Const
import UCap.Domain.Free
import UCap.Domain.PartMap
import UCap.Domain.StaticMap

import Data.Bifunctor
import Data.Biapplicative
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Merge.Strict
-- import Data.Set (Set)
-- import qualified Data.Set as Set

data PartMapG p k g
  = PartMapG { pmgIns :: Map p (GId g)
             , pmgMod :: StaticMapG (p,k) g
             }

deriving instance (Show p, Show k, Show g, Show (GId g)) => Show (PartMapG p k g)
deriving instance (Eq p, Eq k, Eq g, Eq (GId g)) => Eq (PartMapG p k g)
deriving instance (Ord p, Ord k, Ord g, Ord (GId g)) => Ord (PartMapG p k g)

instance (Ord p, Ord k, Semigroup g) => Semigroup (PartMapG p k g) where
  PartMapG im1 mm1 <> PartMapG _ mm2 =
    PartMapG im1 (mm1 <> mm2)

localRead
  :: (Ord p, Ord k, CoordSys g, Ord (GState g), Eq (GEffect g))
  => GId g -> PartMapG p k g -> GCap (PartMapG p k g)
localRead i (PartMapG im mm) =
  let mmc = case capsRead $ localCaps i mm of
              StaticMapC m -> IM.fromMap idC m
              StaticMapUni -> IM.uniform uniC
      imc = IM.fromMap NoChange $ fmap
        (\i' -> if i == i'
                   then NoChange
                   else AnyChange)
        im
  in PartMapC imc mmc

localWrite
  :: (Ord p, Ord k, CoordSys g, Ord (GState g), Eq (GEffect g))
  => GId g -> PartMapG p k g -> GCap (PartMapG p k g)
localWrite i (PartMapG im mm) =
  let mmc = case capsWrite $ localCaps i mm of
              StaticMapC m -> IM.fromMap idC m
              StaticMapUni -> IM.uniform uniC
      imc = IM.fromMap NoChange $ fmap
        (\i' -> if i == i'
                   then AnyChange
                   else NoChange)
        im
  in PartMapC imc mmc

unpackCaps (Caps (PartMapC _ r) (PartMapC _ w)) = 
  let (ru,rf) = IM.toMap r
      (wu,wf) = IM.toMap w
  in Caps (StaticMapC rf) (StaticMapC wf)
  -- in IM.fromMap (Caps ru wu) $ merge
  --      (mapMissing (\_ r -> Caps r idC))
  --      (mapMissing (\_ w -> Caps uniC w))
  --      (zipWithMatched (const Caps))
  --      rf
  --      wf

instance (Ord p, Ord k, Ord (GState g), Eq (GEffect g), CoordSys g)
  => CoordSys (PartMapG p k g) where
  type GCap (PartMapG p k g) = PartMapC p k (GCap g)
  type GId (PartMapG p k g) = GId g
  localCaps i g = Caps (localRead i g) (localWrite i g)
  resolveEffect i (PartMapE me) (PartMapG mgi mge) = 
    let (ss,es) = Map.foldlWithKey
          (\(ss,es) k e -> case e of
                             ConstE s -> (Map.insert k s ss, es)
                             ModifyE e' -> (ss, Map.insert k e' es))
          (Map.empty, Map.empty)
          me
        checkSets = Map.traverseWithKey
                      (\(p,_) _ -> if Map.lookup p mgi == Just i
                                      then Right ()
                                      else Left uniC)
                      ss
        mge' = first (const idC) $ resolveEffect i (StaticMapE es) mge
    in PartMapG <$> (const mgi <$> checkSets) <*> mge'
  resolveCaps i cs (PartMapG mgi mge) =
    let cs' = unpackCaps cs
        e = resolveCaps i cs' mge
    in bimap (fmap (PartMapG mgi)) (PartMapE . fmap ModifyE . unwrapSme) e
  grantRequests i (PartMapG a b) = PartMapG a <$> grantRequests i b
