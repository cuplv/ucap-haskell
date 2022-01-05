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

(*:) :: Editor c1 c2 -> Editor c2 c3 -> Editor c1 c3
(*:) = compEd

_1ed :: (Cap c2) => Editor (c1,c2) c1
_1ed = Editor
  (\(s,_) -> pure s)
  (\c -> (c,uniC))
  (\c -> (c,idC))
  (\e -> (e,idE))

_2ed :: (Cap c1) => Editor (c1,c2) c2
_2ed = Editor
  (\(_,s) -> pure s)
  (\c -> (uniC,c))
  (\c -> (idC,c))
  (\e -> (idE,e))
