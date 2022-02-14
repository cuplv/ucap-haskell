{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module UCap.Replica.Http where

import UCap.Coord
import UCap.Replica.MRep

import Data.Aeson

class
  ( MCS g
  , ToJSON g
  , ToJSON (GCap g)
  , ToJSON (GEffect g)
  , FromJSON g
  , FromJSON (GCap g)
  , FromJSON (GEffect g)
  ) => HttpCS g
