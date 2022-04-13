{-# LANGUAGE FlexibleContexts #-}

module UCap.Op.StaticMap where

import UCap.Domain
import UCap.Domain.StaticMap
import UCap.Lifter
import UCap.Lens
import UCap.Op.Internal

import Control.Arrow

{-| Operate on a particular key. -}
keySLf :: (Cap c, Ord k, Ord (CState c)) => k -> Lifter (StaticMapC k c) c
keySLf k = Lifter
  (^. at k)
  (\rc -> uniC `meet` toKeySmc k rc)
  (\wc -> idC <> toKeySmc k wc)
  (toKeySme k)
