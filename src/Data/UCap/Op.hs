{-# LANGUAGE TypeFamilies #-}

module Data.UCap.Op where

import Data.UCap.Classes

data Op c e s a
  = Op { opRead :: c
       , opWrite :: c
       , opBody :: s -> (e,a)
       }

type Op' c = Op c (CEffect c) (CState c)

instance Functor (Op c e s) where
  fmap f (Op r w b) = Op r w $ (fmap.fmap) f b

instance
  ( Monoid c
  , BMeet c
  , EffectDom e
  , EDState e ~ s
  ) => Applicative (Op c e s) where
  pure a = Op uniC idC (const $ (idE,a))
  Op r1 w1 b1 <*> Op r2 w2 b2 =
    Op (r1 `meet` r2) (w1 <> w2) $ \s ->
      let (e1,f) = b1 s
          (e2,a) = b2 (eFun e1 s)
      in (e2 <> e1, f a)

mkOp :: c -> c -> (CState c -> (CEffect c,a)) -> Op' c a
mkOp = Op

newtype CapFail c = CapFail (Maybe c, Maybe c)

runOp
  :: (Cap c)
  => c
  -> c
  -> CState c
  -> Op' c a
  -> Either (CapFail c) (c,CEffect c,a)
runOp rr rw s (Op r w f) = case (rr <=? r, split rw w) of
  (True, Just rw') -> let (e,a) = f s
                      in Right (rw',e,a)
  (rf, wf) -> Left $ CapFail
    (if rf
        then Nothing
        else Just r
    ,case wf of
        Just _ -> Nothing
        Nothing -> Just w
    )
