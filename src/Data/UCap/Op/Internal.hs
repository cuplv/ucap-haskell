{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Data.UCap.Op.Internal where

import Data.UCap.Classes
import Data.UCap.Editor

import Control.Monad.Except

data OpBody c m a
  = OpBody (CState c -> m (CEffect c, a))

runBody :: OpBody c m a -> CState c -> m (CEffect c, a)
runBody (OpBody b) = b

modEff
  :: (Functor m, Cap c)
  => (CEffect c -> CEffect c)
  -> OpBody c m a
  -> OpBody c m a
modEff f (OpBody b) = OpBody $ \s -> f' <$> b s
  where f' (e,a) = (f e, a)

instance (Functor m) => Functor (OpBody c m) where
  fmap f (OpBody b) = OpBody $ (fmap.fmap.fmap) f b

instance (Monad m, Cap c) => Applicative (OpBody c m) where
  pure = OpBody . const . pure . pure
  OpBody b1 <*> OpBody b2 = OpBody $ \s -> do
    (e1,f) <- b1 s
    (e2,a) <- b2 (eFun e1 s)
    return (e1 <> e2, f a)

instance (Monad m, Cap c) => Monad (OpBody c m) where
  return = pure
  OpBody b1 >>= f = OpBody $ \s -> do
    (e1,a1) <- b1 s
    let OpBody b2 = f a1
    (e2,a2) <- b2 (eFun e1 s)
    return (e1 <> e2, a2)

data Op c a m b
  = Op { opRead :: c
       , opWrite :: c
       , opProduces :: c
       , opBody :: a -> OpBody c m b
       }

instance (Functor m) => Functor (Op c a m) where
  fmap f (Op r w p b) = Op r w p $ (fmap.fmap) f b

instance (Monad m, Cap c) => Applicative (Op c a m) where
  pure a = Op uniC idC idC $ const (pure a) -- (\_ _ -> pure $ (idE, a))

  Op r1 w1 p1 b1 <*> Op r2 w2 p2 b2 = pipe
    (Op r1 w1 p1 $ \a -> (\f -> (a,f)) <$> b1 a)
    (Op r2 w2 p2 $ \(a,f) -> f <$> b2 a)

pipe
  :: (Monad m, Cap c)
  => Op c a1 m a2
  -> Op c a2 m a3
  -> Op c a1 m a3
pipe (Op r1 w1 p1 b1) (Op r2 w2 p2 b2) =
  let (w3,p3) = case (split w2 p1, split p1 w2) of
                  (Just w2',_) -> (w1 <> w2', p2)
                  (_,Just p1') -> (w1, p1' <> p2)
                  _ -> (w1 <> w2, p1 <> p2)
      r3 = r1 `meet` r2
      b3 a = b1 a >>= b2
  in Op r3 w3 p3 b3

(*>>)
  :: (Monad m, Cap c)
  => Op c a1 m a2
  -> Op c a2 m a3
  -> Op c a1 m a3
(*>>) = pipe

mkOp :: (Cap c) => c -> c -> (a -> m (CEffect c, b)) -> Op c a m b
mkOp w p b = Op uniC w p $ \a -> OpBody . const $ (b a)

liftOpM :: (Monad m, MonadTrans t) => Op c a m b -> Op c a (t m) b
liftOpM = onOpM lift

onOpM :: (m1 (CEffect c,b) -> m2 (CEffect c,b)) -> Op c a m1 b -> Op c a m2 b
onOpM g (Op c a m b) = Op c a m $ \a ->
  let OpBody f = b a
  in OpBody $ \s -> g (f s)

mapOp' :: (Functor m, Cap c) => (a -> m b) -> Op c a m b
mapOp' f =
  Op uniC idC idC $ \a -> OpBody . const $ (\b -> (idE, b)) <$> f a

mapOp :: (Applicative m, Cap c) => (a -> b) -> Op c a m b
mapOp f = mapOp' (pure . f)

feedTo :: (Monad m, Cap c) => a1 -> Op c a1 m b -> Op c a2 m b
feedTo a o = pure a *>> o

testOp :: (Monad m, Cap c) => Op c a m Bool -> Op c a (ExceptT () m) ()
testOp o = liftOpM o *>> mapOp' (\b -> if b
                                          then return ()
                                          else throwError ())

queryOp :: (Applicative m, Cap c) => c -> Op c a m (CState c)
queryOp c = Op c idC idC . const . OpBody $ \s -> pure (idE,s)

pairOp :: (Monad m, Cap c) => Op c a m b1 -> Op c a m b2 -> Op c a m (b1,b2)
pairOp o1 o2 = (,) <$> o1 <*> o2

idOp :: (Applicative m, Cap c) => Op c a m a
idOp = mapOp id

effectOp :: (Applicative m, Cap c) => CEffect c -> Op c a m ()
effectOp e = effectOp' e ()

effectOp' :: (Applicative m, Cap c) => CEffect c -> b -> Op c a m b
effectOp' e b = mkOp (mincap e) (undo e) . const . pure $ (e,b)

edLift
  :: (Monad m)
  => Editor c1 c2
  -> Op c2 a m b
  -> Op c1 a (ExceptT () m) b
edLift ed (Op r w p b) = Op
  (readLift ed r)
  (writeLift ed w)
  (writeLift ed p)
  (\a -> edLiftB ed (b a))

edLift' :: (Monad m) => Editor c1 c2 -> Op c2 a m b -> Op c1 a m b
edLift' ed (Op r w p b) = Op
  (readLift ed r)
  (writeLift ed w)
  (writeLift ed p)
  (\a -> edLiftB' ed (b a))

edLiftB
  :: (Monad m)
  => Editor c1 c2
  -> OpBody c2 m a
  -> OpBody c1 (ExceptT () m) a
edLiftB ed (OpBody f) = OpBody $ \s -> case zoomState ed s of
  Just s' -> do
    (e,a) <- lift $ f s'
    return (effLift ed e, a)
  Nothing -> throwError ()

edLiftB'
  :: (Monad m)
  => Editor c1 c2
  -> OpBody c2 m a
  -> OpBody c1 m a
edLiftB' ed o =
  let OpBody f = edLiftB ed o
  in OpBody $ \s -> runExceptT (f s) >>= \case
       Right a -> return a
       Left () -> error "Unhandled editor zoomState failure."
