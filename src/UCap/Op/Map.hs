{-# LANGUAGE FlexibleContexts #-}

module UCap.Op.Map where

import Control.Arrow
import UCap.Domain
import UCap.Lifter
import UCap.Lens
import UCap.Op.Internal

{-| Operate on a particular key. -}
keyLf :: (Eq c, Cap c, Ord k, Ord (CState c)) => k -> Lifter (MapC' k c) c
keyLf k = Lifter
  (^. at k)
  (meetTo $ atMapC k)
  (plusTo $ atMapC k)
  (adjustE k)

{-| Insert a dynamic key-value pair into the map. -}
insertOp
  :: (Eq c, Applicative m, Cap c, Ord k, Ord (CState c), Eq (CEffect c))
  => Op (MapC' k c) m (k,CState c) k
insertOp = mkOp
  insertAny
  idC
  (\(k,v) -> pure (insertE k v, k))

{-| Add a dynamic element to the set. -}
setAdd :: (Ord a, Monad m) => Op (SetC a) m a ()
setAdd = mapOp (\a -> (a, ())) >>> insertOp >>> pure ()
