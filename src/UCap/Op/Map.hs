{-# LANGUAGE FlexibleContexts #-}

module UCap.Op.Map where

import UCap.Domain
import UCap.Lifter
import UCap.Lens
import UCap.Op.Internal

keyLf :: (Cap c, Ord k, Ord (CState c)) => k -> Lifter (MapC' k c) c
keyLf k = Lifter
  (^. at k)
  (meetTo $ atMapC k)
  (plusTo $ atMapC k)
  (adjustE k)

insertOp
  :: (Applicative m, Cap c, Ord k, Ord (CState c), Eq (CEffect c))
  => Op (MapC' k c) m (k,CState c) k
insertOp = mkOp
  insertAny
  idC
  (\(k,v) -> pure (insertE k v, k))
