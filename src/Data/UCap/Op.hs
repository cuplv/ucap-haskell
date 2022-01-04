{-# LANGUAGE TypeFamilies #-}

module Data.UCap.Op where

import Data.UCap.Classes

data OpBody e s a
  = OpBody { opBodyFun :: s -> Maybe (e,a) }

instance Functor (OpBody e s) where
  fmap f (OpBody b) = OpBody $
    (\s -> (\(e,a) -> (e, f a)) <$> b s)

opBFail :: OpBody e s a
opBFail = OpBody $ const Nothing

opBEffect :: e -> OpBody e s ()
opBEffect e = OpBody . const $ Just (e, ())

opBRead :: (Monoid e) => OpBody e s s
opBRead = OpBody $ \s -> Just (idE,s)

opBTest :: (Monoid e) => (s -> Bool) -> OpBody e s ()
opBTest t = OpBody $ \s ->
  if t s
     then Just (idE, ())
     else Nothing

instance (EffectDom e, EDState e ~ s) => Applicative (OpBody e s) where
  pure a = OpBody . const $ Just (idE,a)
  OpBody b1 <*> OpBody b2 = OpBody $ \s -> case b1 s of
    Just (e1,f) -> case b2 (eFun e1 s) of
      Just (e2,a) -> Just (e1 <> e2, f a)
      Nothing -> Nothing
    Nothing -> Nothing

instance (EffectDom e, EDState e ~ s) => Monad (OpBody e s) where
  return = pure
  OpBody b1 >>= m = OpBody $ \s -> case b1 s of
    Just (e1,a1) -> case opBodyFun (m a1) (eFun e1 s) of
      Just (e2,a2) -> Just (e1 <> e2, a2)
      Nothing -> Nothing
    Nothing -> Nothing

data Op c e s a
  = Op { opRead :: c
       , opWrite :: c
       , opProduces :: c
       , opBody :: OpBody e s a
       }

type Op' c = Op c (CEffect c) (CState c)

instance Functor (Op c e s) where
  fmap f (Op r w p b) = Op r w p (fmap f b)

instance
  ( Cap c
  , CEffect c ~ e
  , CState c ~ s
  ) => Applicative (Op c e s) where
  pure a = Op uniC idC idC (pure a)
  Op r1 w1 p1 b1 <*> Op r2 w2 p2 b2 = case (split w2 p1, split p1 w2) of
    (Just w2',_) -> Op (r1 `meet` r2) (w1 <> w2') p2 (b1 <*> b2)
    (_,Just p1') -> Op (r1 `meet` r2) w1 (p1' <> p2) (b1 <*> b2)
    _ -> Op (r1 `meet` r2) (w1 <> w2) (p1 <> p2) (b1 <*> b2)

opFail :: (Cap c) => Op c e s a
opFail = Op uniC idC uniC opBFail

opEffect :: (Cap c, CEffect c ~ e) => e -> Op c e s ()
opEffect e = Op uniC (mincap e) (undo e) (opBEffect e)

opQuery :: (Cap c, Monoid e) => c -> Op c e s s
opQuery c = Op c idC idC opBRead

opTest
  :: (Cap c, CEffect c ~ e, CState c ~ s)
  => c
  -> (s -> Bool)
  -> Op c e s ()
opTest c t = Op c idE idC (opBTest t)

mkOp :: (Monoid c) => c -> c -> (s -> (e,a)) -> Op c e s a
mkOp r w f = Op r w idC (OpBody (Just . f))
