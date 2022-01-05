{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Data.UCap.Op.Either where

import Data.UCap
import Data.UCap.Editor
import Data.UCap.Lens
import Data.UCap.Op.Internal

rightEd
  :: (Cap c1, Cap c2, Ord (CState c1), Ord (CState c2))
  => Editor (EitherC' c1 c2) c2
rightEd = Editor
  (\case
      Right s -> Just s
      Left _ -> Nothing)
  (meetTo $ atR)
  (plusTo $ atR)
  (\e -> OverLR idE e)

leftEd 
  :: (Cap c1, Cap c2, Ord (CState c1), Ord (CState c2))
  => Editor (EitherC' c1 c2) c1
leftEd = Editor
  (\case
      Right _ -> Nothing
      Left s -> Just s)
  (meetTo $ atL)
  (plusTo $ atL)
  (\e -> OverLR e idE)
