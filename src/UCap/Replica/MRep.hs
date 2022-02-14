{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module UCap.Replica.MRep where

import UCap.Domain
import UCap.Coord
import UCap.Replica

import Control.Concurrent.STM.TChan
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

type TBM g e = (String, BMsg g e)

type VC = VClock String

data MRepState g e s
  = MRepState { _hrInitState :: s
              , _hrCoord :: g
              , _hrDag :: VThread String e
              }
  deriving (Show,Eq,Ord)

data MRep g e s
  = MRep { _hrId :: String
         , _hrState :: MRepState g e s
         , _hrInbox :: TChan (TBM g e)
         , _hrSend :: String -> BMsg g e -> IO ()
         }
