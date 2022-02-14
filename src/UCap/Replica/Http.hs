{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module UCap.Replica.Http where

import UCap.Coord

import Data.Aeson

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
