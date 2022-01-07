{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Data.UCap.Op.Internal where

import Data.UCap.Classes
import Data.UCap.Editor

import Control.Monad.Except

{-| An @'OpBody' c m a'@ is an operation kernel, which produces an
  effect (@'CEffect' c@) and return value (@a@), in a monad @m@, based
  on a state value (@'CState' c@). -}
data OpBody c m a
  = OpBody (CState c -> m (CEffect c, a))

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

{-| An operation of type @'Op' c a m b@ is a transaction on a replicated
  capability store of type @c@.  An operation is a read-and-modify
  store update wrapped in a set of capability requirements.

  In addition to reading the store value, the operation takes a
  "dynamic" argument of type @a@, which can affect the update and
  return value, but cannot affect the capability requirements.

  Operations are 'Applicative'.  The operation
  @'Control.Applicative.pure' a@ returns @a@ without reading or
  modifying the store state.
-}
data Op c a m b
  = Op { opRead :: c
       , opWrite :: c
       , opProduces :: c
       , opBody :: a -> OpBody c m b
       }

instance (Functor m) => Functor (Op c a m) where
  fmap f (Op r w p b) = Op r w p $ (fmap.fmap) f b

instance (Monad m, Cap c) => Applicative (Op c a m) where
  pure a = Op uniC idC idC $ const (pure a)

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

{-| The '*>=' ("pipe") operator connects two operations together, so
  that the return value of the first becomes the dynamic input of the
  second.

  The 'idOp' operation is an identity for '*>='.

@
(o *>= idOp) = (idOp *>= o) = o
@
-}
(*>=)
  :: (Monad m, Cap c)
  => Op c a1 m a2
  -> Op c a2 m a3
  -> Op c a1 m a3
(*>=) = pipe

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

{-| Statically fill in the dynamic input of an operation.  The resulting
  operation can be run without piping any further input into it.

@
feedTo a o = pure a *>= o
@
-}
feedTo :: (Monad m, Cap c) => a1 -> Op c a1 m b -> Op c a2 m b
feedTo a o = pure a *>= o

{-| Turn an operation returning 'Bool' into one which fails (using the
  'Control.Monad.Except.ExceptT' monad transformer) when it would have
  returned 'False'.  This is useful for defining assertions that
  should cancel an operation when they don't hold. -}
testOp :: (Monad m, Cap c) => Op c a m Bool -> Op c a (ExceptT () m) ()
testOp o = liftOpM o *>= mapOp' (\b -> if b
                                           then return ()
                                           else throwError ())

{-| @'queryOp' c@ observes the store state, using @c@ as a
  read-requirement to restrict remote interference, and returns the
  read value.
    
  The relationship between the returned value and the actual store
  state depends on the @c@ argument given, so you should prefer to use
  more specific queries defined for particular store types, such as
  'lowerBound' for 'Data.UCap.Counter.CounterC'. -}
queryOp :: (Applicative m, Cap c) => c -> Op c a m (CState c)
queryOp c = Op c idC idC . const . OpBody $ \s -> pure (idE,s)

{-| @'pairOp' o1 o2@ runs operations @o1@ and @o2@ in sequence, giving
  them both the same dynamic input value.  Their results are collected
  in a pair that is returned.

@
'pure' 0 '*>=' ('mapOp' (+ 1) `'pairOp'` 'mapOp' (+ 2)) = 'pure' (1,2)
@

-}
pairOp :: (Monad m, Cap c) => Op c a m b1 -> Op c a m b2 -> Op c a m (b1,b2)
pairOp o1 o2 = (,) <$> o1 <*> o2

idOp :: (Applicative m, Cap c) => Op c a m a
idOp = mapOp id

effectOp :: (Applicative m, Cap c) => CEffect c -> Op c a m ()
effectOp e = effectOp' e ()

effectOp' :: (Applicative m, Cap c) => CEffect c -> b -> Op c a m b
effectOp' e b = mkOp (mincap e) (undo e) . const . pure $ (e,b)

edLift'
  :: (Monad m)
  => Editor c1 c2
  -> Op c2 a m b
  -> Op c1 a (ExceptT () m) b
edLift' ed (Op r w p b) = Op
  (readLift ed r)
  (writeLift ed w)
  (writeLift ed p)
  (\a -> edLiftB' ed (b a))

edLift :: (Monad m) => Editor c1 c2 -> Op c2 a m b -> Op c1 a m b
edLift ed (Op r w p b) = Op
  (readLift ed r)
  (writeLift ed w)
  (writeLift ed p)
  (\a -> edLiftB ed (b a))

edLiftB'
  :: (Monad m)
  => Editor c1 c2
  -> OpBody c2 m a
  -> OpBody c1 (ExceptT () m) a
edLiftB' ed (OpBody f) = OpBody $ \s -> case zoomState ed s of
  Just s' -> do
    (e,a) <- lift $ f s'
    return (effLift ed e, a)
  Nothing -> throwError ()

edLiftB
  :: (Monad m)
  => Editor c1 c2
  -> OpBody c2 m a
  -> OpBody c1 m a
edLiftB ed o =
  let OpBody f = edLiftB' ed o
  in OpBody $ \s -> runExceptT (f s) >>= \case
       Right a -> return a
       Left () -> error "Unhandled editor zoomState failure."
