{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module UCap.Op.Internal where

import UCap.Domain.Classes
import UCap.Lifter

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
  return value, but cannot affect the capability requirements.  This
  allows operations to be chained together in a convenient way (using
  '*>='), even though they do not have a 'Control.Monad.Monad'
  instance.

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

{-| The '*>=' operator connects two operations together, so that the
  return value of the first becomes the dynamic input of the second.
  Their effects run in left-to-right sequence. In @op1 '*>= op2@,
  effects made by @op1@ will modify the store value that is read by
  @op2@.

  The 'idOp' operation is an identity for '*>='.

@
(o '*>=' 'idOp') = ('idOp' '*>=' o) = o
@

  '*>=' is closely related to the generic applicative
  'Control.Applicative.*>' operator, which also sequences two
  operations together but feeds '()' to the second operation rather
  than the first's return value.

@
o1 '*>=' ('feedTo' '()' o2) = o1 '*>' o2
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

{-| 'mapOp' simply returns the dynamic input after transforming it with
  a function.

@
'mapOp' f = f 'Control.Functor.<$>' 'idOp'
@
-}
mapOp :: (Applicative m, Cap c) => (a -> b) -> Op c a m b
mapOp f = mapOp' (pure . f)

{-| Statically fill in the dynamic input of an operation.  The resulting
  operation can be run without piping any further input into it.

@
a `'withInput'` o = 'pure' a '*>=' o

a `'withInput'` 'idOp' = 'pure' a
@
-}
withInput :: (Monad m, Cap c) => a1 -> Op c a1 m b -> Op c a2 m b
withInput a o = pure a *>= o

{-| Turn an operation returning 'Bool' into one which fails (using the
  'Control.Monad.Except.ExceptT' monad transformer) when it would have
  returned 'False'.  This is useful for defining assertions that
  should cancel an operation when they don't hold. -}
assert :: (Monad m, Cap c) => Op c a m Bool -> Op c a (ExceptT () m) ()
assert o = liftOpM o *>= mapOp' (\b -> if b
                                          then return ()
                                          else throwError ())

{-| @'query' c@ observes the store state, using @c@ as a
  read-requirement to restrict remote interference, and returns the
  read value.
    
  The relationship between the returned value and the actual store
  state depends on the @c@ argument given, so you should prefer to use
  more specific queries defined for particular store types, such as
  'Data.UCap.Op.Counter.atLeast' for 'Data.UCap.Counter.CounterC'. -}
query :: (Applicative m, Cap c) => c -> Op c a m (CState c)
query c = Op c idC idC . const . OpBody $ \s -> pure (idE,s)

{-| @'pairOp' o1 o2@ runs operations @o1@ and @o2@ in sequence, giving
  them both the same dynamic input value.  Their results are collected
  in a pair that is returned.  The effects of @o1@ are still visible
  to @o2@.

@
'pure' 0 '*>=' ('mapOp' (+ 1) `'pairOp'` 'mapOp' (+ 2)) = 'pure' (1,2)
@

-}
pairOp :: (Monad m, Cap c) => Op c a m b1 -> Op c a m b2 -> Op c a m (b1,b2)
pairOp o1 o2 = (,) <$> o1 <*> o2

{-| The identity operation, which leaves the store untouched and simply
  returns its dynamic input. -}
idOp :: (Applicative m, Cap c) => Op c a m a
idOp = mapOp id

{-| Modify the store with a static effect.  Because the effect is
  determined by a static argument (not based on the store state or a
  dynamic input), the operation @'effect' e@ automatically has the
  minimum write-requirement for the effect, @'mincap' e@.

  'effect' passes its input on as the return value.
-}
effect :: (Applicative m, Cap c) => CEffect c -> Op c a m a
effect e = mkOp (mincap e) (undo e) $ \a -> pure (e,a)
-- effect e = effect' e ()

{-| Same as 'effect', but takes a static return value as well. -}
effect' :: (Applicative m, Cap c) => CEffect c -> b -> Op c a m b
effect' e b = mkOp (mincap e) (undo e) . const . pure $ (e,b)

overLf'
  :: (Monad m)
  => Lifter c1 c2
  -> Op c2 a m b
  -> Op c1 a (ExceptT () m) b
overLf' ed (Op r w p b) = Op
  (readLift ed r)
  (writeLift ed w)
  (writeLift ed p)
  (\a -> edLiftB' ed (b a))

{-| Given state types @c1@ and @c2@, where @c2@ is a component of @c1@,
  'edLift' transforms an operation on @c2@ into the equivalent @c1@
  operation which leaves the other components untouched. -}
overLf :: (Monad m) => Lifter c1 c2 -> Op c2 a m b -> Op c1 a m b
overLf ed (Op r w p b) = Op
  (readLift ed r)
  (writeLift ed w)
  (writeLift ed p)
  (\a -> edLiftB ed (b a))

edLiftB'
  :: (Monad m)
  => Lifter c1 c2
  -> OpBody c2 m a
  -> OpBody c1 (ExceptT () m) a
edLiftB' ed (OpBody f) = OpBody $ \s -> case zoomState ed s of
  Just s' -> do
    (e,a) <- lift $ f s'
    return (effLift ed e, a)
  Nothing -> throwError ()

edLiftB
  :: (Monad m)
  => Lifter c1 c2
  -> OpBody c2 m a
  -> OpBody c1 m a
edLiftB ed o =
  let OpBody f = edLiftB' ed o
  in OpBody $ \s -> runExceptT (f s) >>= \case
       Right a -> return a
       Left () -> error "Unhandled editor zoomState failure."

data Caps c
  = Caps { capsRead :: c
         , capsWrite :: c
         }

emptyCaps :: (Cap c) => Caps c
emptyCaps = Caps uniC idC

fullCaps :: (Cap c) => Caps c
fullCaps = Caps idC uniC

(^#) :: (Monad m) => Lifter c1 c2 -> Op c2 a m b -> Op c1 a m b
(^#) = overLf