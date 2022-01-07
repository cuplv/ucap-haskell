{-# LANGUAGE TypeFamilies #-}

module Data.UCap.Op
  (
  -- * Operations
    Op
  , (*>=)
  , idOp
  , queryOp
  , effectOp
  , effectOp'
  , feedTo
  , mapOp
  , mapOp'
  , testOp
  -- * Lifting Operations
  , Editor
  , (*:)
  , edLift
  , edLift'
  -- * Operations for Counter
  , module Data.UCap.Op.Counter
  -- * Editors for Either
  , module Data.UCap.Op.Either
  -- * Operations and Editors for Map
  , module Data.UCap.Op.Map
  ) where

import Data.UCap.Editor
import Data.UCap.Op.Counter
import Data.UCap.Op.Either
import Data.UCap.Op.Internal
import Data.UCap.Op.Map
