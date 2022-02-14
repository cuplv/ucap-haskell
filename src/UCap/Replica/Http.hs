{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module UCap.Replica.Http where

import UCap.Domain
import UCap.Coord
import UCap.Replica

import Data.Aeson
import GHC.Generics

class
  ( GId g ~ String
  , CoordSys g
  , ToJSON g
  , ToJSON (GCap g)
  , ToJSON (GEffect g)
  , FromJSON g
  , FromJSON (GCap g)
  , FromJSON (GEffect g)
  ) => HttpCS g

data ESeq e
  = ESeq { eseqPayload :: [(VC, e)]
         }
  deriving (Show,Eq,Ord,Generic)

instance (ToJSON e) => ToJSON (ESeq e)
instance (FromJSON e) => FromJSON (ESeq e)

data BMsg g e
  = Hello VC
  | BCast (ESeq e) g
  | ERequest Int
  deriving (Show,Eq,Ord,Generic)

instance (ToJSON e, ToJSON g) => ToJSON (BMsg g e)
instance (FromJSON e, FromJSON g) => FromJSON (BMsg g e)

type BMsg' g = BMsg g (GEffect g)

type VC = VClock String
