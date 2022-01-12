{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.UCap.Editor where

import Data.UCap.Classes

data Editor c1 c2
  = Editor { zoomState :: CState c1 -> Maybe (CState c2)
           , readLift :: c2 -> c1
           , writeLift :: c2 -> c1
           , effLift :: CEffect c2 -> CEffect c1
           }

compEd :: Editor c1 c2 -> Editor c2 c3 -> Editor c1 c3
compEd (Editor z1 r1 w1 e1) (Editor z2 r2 w2 e2) = Editor
  (\s -> z1 s >>= z2)
  (r1 . r2)
  (w1 . w2)
  (e1 . e2)

idEd :: Editor c c
idEd = Editor pure id id id

(>:) :: Editor c1 c2 -> Editor c2 c3 -> Editor c1 c3
(>:) = compEd

class EdIdx1 c1 c2 where
  _1ed' :: Editor c1 c2

instance (Cap c1, Cap c2) => EdIdx1 (c1,c2) c1 where
  _1ed' = Editor
    (\(s,_) -> pure s)
    (\c -> (c,uniC))
    (\c -> (c,idC))
    (\e -> (e,idE))

instance (Cap c1, Cap c2, Cap c3) => EdIdx1 (c1,c2,c3) c1 where
  _1ed' = Editor
    (\(s,_,_) -> pure s)
    (\c -> (c,uniC,uniC))
    (\c -> (c,idC,idC))
    (\e -> (e,idE,idE))

instance (Cap c1, Cap c2, Cap c3, Cap c4) => EdIdx1 (c1,c2,c3,c4) c1 where
  _1ed' = Editor
    (\(s,_,_,_) -> pure s)
    (\c -> (c,uniC,uniC,uniC))
    (\c -> (c,idC,idC,idC))
    (\e -> (e,idE,idE,idE))

class EdIdx2 c1 c2 where
  _2ed' :: Editor c1 c2

instance (Cap c1, Cap c2) => EdIdx2 (c1,c2) c2 where
  _2ed' = Editor
    (\(_,s) -> pure s)
    (\c -> (uniC,c))
    (\c -> (idC,c))
    (\e -> (idE,e))

instance (Cap c1, Cap c2, Cap c3) => EdIdx2 (c1,c2,c3) c2 where
  _2ed' = Editor
    (\(_,s,_) -> pure s)
    (\c -> (uniC,c,uniC))
    (\c -> (idC,c,idC))
    (\e -> (idE,e,idE))

instance (Cap c1, Cap c2, Cap c3, Cap c4) => EdIdx2 (c1,c2,c3,c4) c2 where
  _2ed' = Editor
    (\(_,s,_,_) -> pure s)
    (\c -> (uniC,c,uniC,uniC))
    (\c -> (idC,c,idC,idC))
    (\e -> (idE,e,idE,idE))

class EdIdx3 c1 c2 where
  _3ed' :: Editor c1 c2

instance (Cap c1, Cap c2, Cap c3) => EdIdx3 (c1,c2,c3) c3 where
  _3ed' = Editor
    (\(_,_,s) -> pure s)
    (\c -> (uniC,uniC,c))
    (\c -> (idC,idC,c))
    (\e -> (idE,idE,e))

instance (Cap c1, Cap c2, Cap c3, Cap c4) => EdIdx3 (c1,c2,c3,c4) c3 where
  _3ed' = Editor
    (\(_,_,s,_) -> pure s)
    (\c -> (uniC,uniC,c,uniC))
    (\c -> (idC,idC,c,idC))
    (\e -> (idE,idE,e,idE))

class EdIdx4 c1 c2 where
  _4ed' :: Editor c1 c2

instance (Cap c1, Cap c2, Cap c3, Cap c4) => EdIdx4 (c1,c2,c3,c4) c4 where
  _4ed' = Editor
    (\(_,_,_,s) -> pure s)
    (\c -> (uniC,uniC,uniC,c))
    (\c -> (idC,idC,idC,c))
    (\e -> (idE,idE,idE,e))

-- | Editor for the first member of a tuple or similar structure.
_1ed :: (EdIdx1 c1 c2) => Editor c1 c2
_1ed = _1ed'

-- | Editor for the second member of a tuple or similar structure.
_2ed :: (EdIdx2 c1 c2) => Editor c1 c2
_2ed = _2ed'

-- | Editor for the third member of a tuple or similar structure.
_3ed :: (EdIdx3 c1 c2) => Editor c1 c2
_3ed = _3ed'

-- | Editor for the fourth member of a tuple or similar structure.
_4ed :: (EdIdx4 c1 c2) => Editor c1 c2
_4ed = _4ed'
