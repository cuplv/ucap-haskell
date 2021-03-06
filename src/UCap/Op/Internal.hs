{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module UCap.Op.Internal where

import UCap.Domain.Classes
import UCap.Lifter

import Prelude hiding (id,(.))

import Control.Arrow
import Control.Category
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

{-| An operation of type @'Op' c m a b@ is a transaction on a replicated
  capability store of type @c@.  An operation is a read-and-modify
  store update wrapped in a set of capability requirements.

  In addition to reading the store value, the operation takes a
  "dynamic" argument of type @a@, which can affect the update and
  return value, but cannot affect the capability requirements.  This
  allows operations to be chained together in a convenient way (using
  'pipe' or 'Control.Arrow.>>>'), even though they do not have a
  'Control.Monad.Monad' instance.

  Operations are 'Applicative'.  The operation
  @'Control.Applicative.pure' a@ returns @a@ without reading or
  modifying the store state.

  Operations also implement 'Control.Arrrow.Arrow', so they can be
  composed using the other arrow combinators (such as
  'Control.Arrrow.<<<' and 'Control.Arrow.***') in addition to
  'Control.Arrow.>>>'.
-}
data Op c m a b
  = Op { opRead :: c
       , opWrite :: c
       , opProduces :: c
       , opBody :: a -> OpBody c m b
       }

instance (Functor m) => Functor (Op c m a) where
  fmap f (Op r w p b) = Op r w p $ (fmap.fmap) f b

instance (Monad m, Cap c) => Applicative (Op c m a) where
  pure a = Op uniC idC idC $ const (pure a)

  Op r1 w1 p1 b1 <*> Op r2 w2 p2 b2 = pipe
    (Op r1 w1 p1 $ \a -> (\f -> (a,f)) <$> b1 a)
    (Op r2 w2 p2 $ \(a,f) -> f <$> b2 a)

instance (Monad m, Cap c) => Category (Op c m) where
  id = idOp
  (.) = flip pipe

instance (Monad m, Cap c) => Arrow (Op c m) where
  arr = mapOp
  first o = pairOp (mapOp fst `pipe` o) (mapOp snd)
  second o = pairOp (mapOp fst) (mapOp snd `pipe` o)
  o1 *** o2 = pairOp (mapOp fst `pipe` o1) (mapOp snd `pipe` o2)
  (&&&) = pairOp

instance (Monad m, Cap c) => ArrowChoice (Op c m) where
  (+++) = eitherOp

{-| The right-to-left version of 'pipe'. -}
pipeRL
  :: (Monad m, Cap c)
  => Op c m a2 a3
  -> Op c m a1 a2
  -> Op c m a1 a3
pipeRL (Op r2 w2 p2 b2) (Op r1 w1 p1 b1) =
  let (w3,p3) = case (split w2 p1, split p1 w2) of
                  (Right w2',_) -> (w1 <> w2', p2)
                  (_,Right p1') -> (w1, p1' <> p2)
                  _ -> (w1 <> w2, p1 <> p2)
      r3 = r1 `meet` r2
      b3 a = b1 a >>= b2
  in Op r3 w3 p3 b3

{-| The 'pipe' combinator connects two operations together, so that the
  return value of the first becomes the dynamic input of the second.
  Their effects run in left-to-right sequence. In @'pipe' op1 op2@,
  effects made by @op1@ will modify the store value that is read by
  @op2@.  'pipe' is a synonym for the '>>>' arrow operator, which most
  examples will use.

  The 'idOp' operation is an identity for 'pipe'/'>>>'.

@
(o '>>>' 'idOp') = ('idOp' '>>>' o) = o
@

  '>>>' is closely related to the generic applicative
  'Control.Applicative.*>' operator, which also sequences two
  operations together but feeds the same outer input value to both,
  discarding the return value of the first.

@
('withInput' a o1) '>>>' ('withInput' a o2) = withInput a (o1 '*>' o2)
@
-}
pipe
  :: (Monad m, Cap c)
  => Op c m a1 a2
  -> Op c m a2 a3
  -> Op c m a1 a3
pipe = flip pipeRL

mkOp :: (Cap c) => c -> c -> (a -> m (CEffect c, b)) -> Op c m a b
mkOp w p b = Op uniC w p $ \a -> OpBody . const $ (b a)

liftOpM :: (Monad m, MonadTrans t) => Op c m a b -> Op c (t m) a b
liftOpM = onOpM lift

onOpM :: (m1 (CEffect c,b) -> m2 (CEffect c,b)) -> Op c m1 a b -> Op c m2 a b
onOpM g (Op c m a b) = Op c m a $ \a ->
  let OpBody f = b a
  in OpBody $ \s -> g (f s)

{-| 'actionOp' runs an action in the underlying monad, using the output
  as the operation's output.  Like 'mapOp', 'action' leaves the store
  state untouched.

  For example, the following operation prints the current state of the
  store and returns @()@.

@
'query' 'uniC' '>>>' 'actionOp' 'print'
@
-}
actionOp :: (Functor m, Cap c) => (a -> m b) -> Op c m a b
actionOp f =
  Op uniC idC idC $ \a -> OpBody . const $ (\b -> (idE, b)) <$> f a

{-| 'mapOp' simply returns the dynamic input after transforming it with
  a function.  This is a synonym for 'Control.Arrow.arr'.

@
'mapOp' f = f 'Control.Functor.<$>' 'idOp'
@
-}
mapOp :: (Applicative m, Cap c) => (a -> b) -> Op c m a b
mapOp f = actionOp (pure . f)

{-| Statically fill in the dynamic input of an operation.  The resulting
  operation can be run without piping any further input into it.

@
a `'withInput'` o = 'pure' a '>>>' o

a `'withInput'` 'idOp' = 'pure' a
@
-}
withInput :: (Monad m, Cap c) => a1 -> Op c m a1 b -> Op c m a2 b
withInput a o = pure a >>> o

{-| Turn an operation returning 'Bool' into one which fails (using the
  'Control.Monad.Except.ExceptT' monad transformer) when it would have
  returned 'False'.  This is useful for defining assertions that
  should cancel an operation when they don't hold. -}
assert :: (Monad m, Cap c) => Op c m a Bool -> Op c (ExceptT () m) a ()
assert o = liftOpM o >>> actionOp (\b -> if b
                                            then return ()
                                            else throwError ())

{-| @'query' c@ observes the store state, using @c@ as a
  read-requirement to restrict remote interference, and returns the
  read value.
    
  The relationship between the returned value and the actual store
  state depends on the @c@ argument given, so you should prefer to use
  more specific queries defined for particular store types, such as
  'Data.UCap.Op.Counter.atLeast' for 'Data.UCap.Counter.CounterC'. -}
query :: (Applicative m, Cap c) => c -> Op c m a (CState c)
query c = Op c idC idC . const . OpBody $ \s -> pure (idE,s)

{-| @'pairOp' o1 o2@ runs operations @o1@ and @o2@ in sequence, giving
  them both the same dynamic input value.  Their results are collected
  in a pair that is returned.  The effects of @o1@ are still visible
  to @o2@.

  This is a synonym for the 'Control.Arrrow.&&&' arrow operator.

@
'pure' 0 '>>>' ('mapOp' (+ 1) `'pairOp'` 'mapOp' (+ 2)) = 'pure' (1,2)
@

-}
pairOp :: (Monad m, Cap c) => Op c m a b1 -> Op c m a b2 -> Op c m a (b1,b2)
pairOp o1 o2 = (,) <$> o1 <*> o2

{-| Given an 'Data.Either.Either' input, apply the one of the given
  operations to it, based on the 'Data.Either.Left' or
  'Data.Either.Right' value inside.  This is a synonym for the '+++'
  arrow operator.

@
'withInput' ('Data.Either.Left' x) op1 op2 = 'withInput' x op1

'withInput' ('Data.Either.Right' x) op1 op2 = 'withInput' x op2
@
-}
eitherOp
  :: (Functor m, Cap c)
  => Op c m a1 b1
  -> Op c m a2 b2
  -> Op c m (Either a1 a2) (Either b1 b2)
eitherOp (Op r1 w1 p1 b1) (Op r2 w2 p2 b2) = Op
  (r1 `meet` r2)
  (w1 <> w2)
  (p1 <> p2)
  (either (fmap Left . b1) (fmap Right . b2))

{-| The identity operation, which leaves the store untouched and simply
  returns its dynamic input. -}
idOp :: (Applicative m, Cap c) => Op c m a a
idOp = mapOp id

{-| Modify the store with a static effect.  Because the effect is
  determined by a static argument (not based on the store state or a
  dynamic input), the operation @'effect' e@ automatically has the
  minimum write-requirement for the effect, @'mincap' e@.

  'effect' passes its input on as the return value.
-}
effect :: (Applicative m, Cap c) => CEffect c -> Op c m a a
effect e = mkOp (mincap e) (undo e) $ \a -> pure (e,a)

{-| Same as 'effect', but takes a static return value as well. -}
effect' :: (Applicative m, Cap c) => CEffect c -> b -> Op c m a b
effect' e b = mkOp (mincap e) (undo e) . const . pure $ (e,b)

overLf'
  :: (Monad m)
  => Lifter c1 c2
  -> Op c2 m a b
  -> Op c1 (ExceptT () m) a b
overLf' ed (Op r w p b) = Op
  (readLift ed r)
  (writeLift ed w)
  (writeLift ed p)
  (\a -> edLiftB' ed (b a))

{-| Given state types @c1@ and @c2@, where @c2@ is a component of @c1@,
  'edLift' transforms an operation on @c2@ into the equivalent @c1@
  operation which leaves the other components untouched. -}
overLf :: (Monad m) => Lifter c1 c2 -> Op c2 m a b -> Op c1 m a b
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

{- | Infix version of 'overLf'.

@
'UCap.Lifter._1ed' ^# op1 = overLf 'UCap.Lifter._1ed' op1
@
-}
(^#) :: (Monad m) => Lifter c1 c2 -> Op c2 m a b -> Op c1 m a b
(^#) = overLf
