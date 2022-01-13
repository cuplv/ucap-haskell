{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module UCap.Op.Either where

import UCap.Domain
import UCap.Lifter
import UCap.Lens
import UCap.Op.Internal

rightLf
  :: (Cap c1, Cap c2, Ord (CState c1), Ord (CState c2))
  => Lifter (EitherC' c1 c2) c2
rightLf = Lifter
  (\case
      Right s -> Just s
      Left _ -> Nothing)
  (meetTo $ atR)
  (plusTo $ atR)
  (\e -> OverLR idE e)

leftLf 
  :: (Cap c1, Cap c2, Ord (CState c1), Ord (CState c2))
  => Lifter (EitherC' c1 c2) c1
leftLf = Lifter
  (\case
      Right _ -> Nothing
      Left s -> Just s)
  (meetTo $ atL)
  (plusTo $ atL)
  (\e -> OverLR e idE)
