{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module UCap.Replica.Types where

import Control.Monad (filterM)

class (Ord (DKey g)) => IsKV g where
  type DKey g
  type DVal g

class (Monad m, IsKV g) => Dag g m where
  backtrack :: DKey g -> g -> m (Maybe [DKey g])
  backtrack k g = parents k g >>= \case
    Just ps -> Just <$> narrow g ps
    Nothing -> return Nothing
  narrow :: g -> [DKey g] -> m [DKey g]
  narrow g ks =
    let f i = not . or <$> traverse (descendant g i) ks
    in filterM f ks
  payload :: DKey g -> g -> m (Maybe (DVal g))
  descendant :: g -> DKey g -> DKey g -> m Bool
  parents :: DKey g -> g -> m (Maybe [DKey g])
  ends :: g -> m [DKey g]
