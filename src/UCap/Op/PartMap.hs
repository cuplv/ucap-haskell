{-# LANGUAGE FlexibleContexts #-}

module UCap.Op.PartMap where

import UCap.Domain
import UCap.Domain.PartMap
import UCap.Lifter
import UCap.Lens
import UCap.Op.Internal

import Control.Arrow

{-| Operate on a particular key. -}
keyPLf
  :: (Cap c, Ord p, Ord k, Ord (CState c))
  => (p,k)
  -> Lifter (PartMapC p k c) c
keyPLf k = Lifter
  (^. at k)
  (\rc -> uniC `meet` capPmc k rc)
  (\wc -> idC <> capPmc k wc)
  (modPme k)

{-| Insert into the map, in a static partition, using a dynamic key and
  value. -}
insPmeOp
  :: (Ord p, Applicative m, Cap c, Ord k, Ord (CState c), Eq (CEffect c))
  => p
  -> Op (PartMapC p k c) m (k, CState c) (p,k)
insPmeOp p = mkOp
  (insPmc p)
  idC
  (\(k,v) -> pure (insPme (p,k) v, (p,k)))
