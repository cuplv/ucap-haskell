{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module UCap.Lifter where

import UCap.Domain.Classes

data Lifter c1 c2
  = Lifter { zoomState :: CState c1 -> Maybe (CState c2)
           , readLift :: c2 -> c1
           , writeLift :: c2 -> c1
           , effLift :: CEffect c2 -> CEffect c1
           }

compLf :: Lifter c1 c2 -> Lifter c2 c3 -> Lifter c1 c3
compLf (Lifter z1 r1 w1 e1) (Lifter z2 r2 w2 e2) = Lifter
  (\s -> z1 s >>= z2)
  (r1 . r2)
  (w1 . w2)
  (e1 . e2)

idLf :: Lifter c c
idLf = Lifter pure id id id

(>:) :: Lifter c1 c2 -> Lifter c2 c3 -> Lifter c1 c3
(>:) = compLf

class LfIdx1 c1 c2 where
  _1ed' :: Lifter c1 c2

instance (Cap c1, Cap c2) => LfIdx1 (c1,c2) c1 where
  _1ed' = Lifter
    (\(s,_) -> pure s)
    (\c -> (c,uniC))
    (\c -> (c,idC))
    (\e -> (e,idE))

instance (Cap c1, Cap c2, Cap c3) => LfIdx1 (c1,c2,c3) c1 where
  _1ed' = Lifter
    (\(s,_,_) -> pure s)
    (\c -> (c,uniC,uniC))
    (\c -> (c,idC,idC))
    (\e -> (e,idE,idE))

instance (Cap c1, Cap c2, Cap c3, Cap c4) => LfIdx1 (c1,c2,c3,c4) c1 where
  _1ed' = Lifter
    (\(s,_,_,_) -> pure s)
    (\c -> (c,uniC,uniC,uniC))
    (\c -> (c,idC,idC,idC))
    (\e -> (e,idE,idE,idE))

class LfIdx2 c1 c2 where
  _2ed' :: Lifter c1 c2

instance (Cap c1, Cap c2) => LfIdx2 (c1,c2) c2 where
  _2ed' = Lifter
    (\(_,s) -> pure s)
    (\c -> (uniC,c))
    (\c -> (idC,c))
    (\e -> (idE,e))

instance (Cap c1, Cap c2, Cap c3) => LfIdx2 (c1,c2,c3) c2 where
  _2ed' = Lifter
    (\(_,s,_) -> pure s)
    (\c -> (uniC,c,uniC))
    (\c -> (idC,c,idC))
    (\e -> (idE,e,idE))

instance (Cap c1, Cap c2, Cap c3, Cap c4) => LfIdx2 (c1,c2,c3,c4) c2 where
  _2ed' = Lifter
    (\(_,s,_,_) -> pure s)
    (\c -> (uniC,c,uniC,uniC))
    (\c -> (idC,c,idC,idC))
    (\e -> (idE,e,idE,idE))

class LfIdx3 c1 c2 where
  _3ed' :: Lifter c1 c2

instance (Cap c1, Cap c2, Cap c3) => LfIdx3 (c1,c2,c3) c3 where
  _3ed' = Lifter
    (\(_,_,s) -> pure s)
    (\c -> (uniC,uniC,c))
    (\c -> (idC,idC,c))
    (\e -> (idE,idE,e))

instance (Cap c1, Cap c2, Cap c3, Cap c4) => LfIdx3 (c1,c2,c3,c4) c3 where
  _3ed' = Lifter
    (\(_,_,s,_) -> pure s)
    (\c -> (uniC,uniC,c,uniC))
    (\c -> (idC,idC,c,idC))
    (\e -> (idE,idE,e,idE))

class LfIdx4 c1 c2 where
  _4ed' :: Lifter c1 c2

instance (Cap c1, Cap c2, Cap c3, Cap c4) => LfIdx4 (c1,c2,c3,c4) c4 where
  _4ed' = Lifter
    (\(_,_,_,s) -> pure s)
    (\c -> (uniC,uniC,uniC,c))
    (\c -> (idC,idC,idC,c))
    (\e -> (idE,idE,idE,e))

-- | Lifter for the first member of a tuple or similar structure.
_1ed :: (LfIdx1 c1 c2) => Lifter c1 c2
_1ed = _1ed'

-- | Lifter for the second member of a tuple or similar structure.
_2ed :: (LfIdx2 c1 c2) => Lifter c1 c2
_2ed = _2ed'

-- | Lifter for the third member of a tuple or similar structure.
_3ed :: (LfIdx3 c1 c2) => Lifter c1 c2
_3ed = _3ed'

-- | Lifter for the fourth member of a tuple or similar structure.
_4ed :: (LfIdx4 c1 c2) => Lifter c1 c2
_4ed = _4ed'
