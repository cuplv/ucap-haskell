{-# LANGUAGE FlexibleContexts #-}

module Data.UCap.Op.Map where

import Data.UCap
import Data.UCap.Editor
import Data.UCap.Lens
import Data.UCap.Op.Internal

keyEd :: (Cap c, Ord k, Ord (CState c)) => k -> Editor (MapC' k c) c
keyEd k = Editor
  (^. at k)
  (meetTo $ atMapC k)
  (plusTo $ atMapC k)
  (adjustE k)

insertOp
  :: (Applicative m, Cap c, Ord k, Ord (CState c), Eq (CEffect c))
  => Op (MapC' k c) (k,CState c) m k
insertOp = mkOp
  insertAny
  idC
  (\(k,v) -> pure (insertE k v, k))
