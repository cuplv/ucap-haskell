{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Data.UCap.Op.Internal where

import Data.UCap.Classes
import Data.UCap.Editor
import Data.UCap.Lens

data OpBody e s a
  = OpBody { opBodyFun :: s -> Maybe (e,a) }

type OpBody' c a = OpBody (CEffect c) (CState c) a

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

data Op c a
  = Op { opRead :: c
       , opWrite :: c
       , opProduces :: c
       , opBody :: a
       }

type Op' c a = Op c (OpBody' c a)

type PreOp c a b = Op c (a -> OpBody' c b)

instance Functor (Op c) where
  fmap f (Op r w p b) = Op r w p (f b)

instance (Cap c) => Applicative (Op c) where
  pure a = Op uniC idC idC a
  Op r1 w1 p1 b1 <*> Op r2 w2 p2 b2 = case (split w2 p1, split p1 w2) of
    (Just w2',_) -> Op (r1 `meet` r2) (w1 <> w2') p2 (b1 b2)
    (_,Just p1') -> Op (r1 `meet` r2) w1 (p1' <> p2) (b1 b2)
    _ -> Op (r1 `meet` r2) (w1 <> w2) (p1 <> p2) (b1 b2)

opFail :: (Cap c) => Op' c a
opFail = Op uniC idC uniC opBFail

opEffect :: (Cap c) => CEffect c -> Op' c ()
opEffect e = Op uniC (mincap e) (undo e) (opBEffect e)

opQuery :: (Cap c) => c -> Op' c (CState c)
opQuery c = Op c idC idC opBRead

opTest
  :: (Cap c)
  => c
  -> (CState c -> Bool)
  -> Op' c ()
opTest c t = Op c idE idC (opBTest t)

mkOp :: (Monoid c) => c -> c -> (s -> (e,a)) -> Op c (OpBody e s a)
mkOp r w f = Op r w idC (OpBody (Just . f))

mkPre :: (Monoid c) => c -> c -> (a -> OpBody' c b) -> PreOp c a b
mkPre r w fo = Op r w idC fo

(<*>=) :: (Applicative f, Monad g) => f (g a) -> f (a -> g b) -> f (g b)
(<*>=) m f = fmap (>>=) m <*> f

edLift :: Editor c1 c2 -> Op' c2 a -> Op' c1 a
edLift ed (Op r w p b) = Op
  (readLift ed r)
  (writeLift ed w)
  (writeLift ed p)
  (edLiftB ed b)

edLiftB :: Editor c1 c2 -> OpBody' c2 a -> OpBody' c1 a
edLiftB ed (OpBody f) = OpBody $ \s ->
  case zoomState ed s >>= f of
    Just (e,a) -> Just (effLift ed e, a)
    Nothing -> Nothing

edLiftP :: Editor c1 c2 -> PreOp c2 a b -> PreOp c1 a b
edLiftP ed (Op r w p mf) = Op
  (readLift ed r)
  (writeLift ed w)
  (writeLift ed p)
  (edLiftB ed . mf)
