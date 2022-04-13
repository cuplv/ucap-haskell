{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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

import Control.Monad (foldM)
import Data.Aeson
import Data.Bifunctor
import Data.Biapplicative
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Merge.Strict
import GHC.Generics

data PartMapG p k i c
  = PartMapG { pmgIns :: Map p i
             , pmgMod :: StaticMapG (p,k) (TokenG i c)
             }
  deriving (Show,Eq,Ord,Generic)

instance (Ord i, Ord p, Ord k, Semigroup c) => Semigroup (PartMapG p k i c) where
  PartMapG im1 mm1 <> PartMapG _ mm2 =
    PartMapG im1 (mm1 <> mm2)

instance (ToJSONKey p, ToJSON p, ToJSONKey k, ToJSON k, ToJSONKey i, ToJSON i, ToJSON c) => ToJSON (PartMapG p k i c) where
  toEncoding = genericToEncoding defaultOptions
instance (Ord p, FromJSONKey p, FromJSON p, Ord k, FromJSONKey k, FromJSON k, Ord i, FromJSONKey i, FromJSON i, FromJSON c) => FromJSON (PartMapG p k i c)

localRead
  :: (Ord i, Ord p, Ord k, Cap c, Ord (CState c), Eq (CEffect c))
  => i -> PartMapG p k i c -> GCap (PartMapG p k i c)
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
  :: (Ord i, Ord p, Ord k, Cap c, Ord (CState c), Eq (CEffect c))
  => i -> PartMapG p k i c -> GCap (PartMapG p k i c)
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

instance (Ord i, Ord p, Ord k, Cap c, Ord (CState c), Eq (CEffect c))
  => CoordSys (PartMapG p k i c) where
  type GCap (PartMapG p k i c) = PartMapC p k c
  type GId (PartMapG p k i c) = i

  localCaps i g = Caps (localRead i g) (localWrite i g)

  resolveEffect i (PartMapE me) g = foldM
    (\(PartMapG mgi (StaticMapG mge)) ((p,k),e) -> case e of
       ConstE s -> case Map.lookup p mgi of
          Just i' | i == i' -> 
            let mge' = Map.insert (p,k) (mkTokenG i) mge
            in Right $ PartMapG mgi (StaticMapG mge')
          Nothing -> Left idC
       ModifyE e' ->
         let mge' = StaticMapG <$> Map.alterF
               (\case
                   Just g -> bimap (capPmc (p,k)) Just $ resolveEffect i e' g
                   Nothing -> Left (mincap (modPme (p,k) e')))
               (p,k)
               mge
         in PartMapG mgi <$> mge')
    g
    (Map.assocs me)

  resolveCaps i cs (PartMapG mgi mge) =
    let cs' = unpackCaps cs
        e = resolveCaps i cs' mge
    in bimap (fmap (PartMapG mgi)) (PartMapE . fmap ModifyE . unwrapSme) e
  grantRequests i (PartMapG a b) = PartMapG a <$> grantRequests i b

emptyPartMapG :: (Ord i, Ord k) => [i] -> PartMapG i k i c
emptyPartMapG is = PartMapG
  (Map.fromList (map (\i -> (i,i)) is))
  (initStaticMapG [])
