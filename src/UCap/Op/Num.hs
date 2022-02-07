module UCap.Op.Num where

import UCap.Domain
import UCap.Op.Internal

import Control.Monad.Except

upperBound :: (Applicative m, Integral n, Ord n) => Op (CounterC n) m a n
upperBound = query subAny

atMost :: (Monad m, Integral n, Ord n) => Op (CounterC n) m n Bool
atMost = pairOp idOp upperBound `pipe` mapOp (\(x,s) -> x >= s)

lowerBound :: (Applicative m, Integral n, Ord n) => Op (CounterC n) m a n
lowerBound = query addAny

atLeast :: (Monad m, Integral n, Ord n) => Op (CounterC n) m n Bool
atLeast = pairOp idOp lowerBound `pipe` mapOp (\(x,s) -> x <= s)

addOp :: (Applicative m, Integral n, Ord n) => n -> Op (CounterC n) m a n
addOp n = effect' (addE n) n

addOp' :: (Applicative m, Integral n, Ord n) => Op (CounterC n) m n n
addOp' = Op uniC addAny idC $ \n -> OpBody $ \_ -> pure (addE n, n)

subOp :: (Applicative m, Integral n, Ord n) => n -> Op (CounterC n) m a n
subOp n = effect' (subE n) n

subOp' :: (Applicative m, Integral n, Ord n) => Op (CounterC n) m n ()
subOp' = Op uniC subAny idC $ \n -> OpBody $ \_ -> pure (subE n, ())

subGuard
  :: (Monad m, Integral n, Ord n)
  => n
  -> n
  -> Op (CounterC n) (ExceptT () m) a n
subGuard lim amt = assert (withInput lim atLeast) *> subOp amt

addGuard
  :: (Monad m, Integral n, Ord n)
  => n
  -> n
  -> Op (CounterC n) (ExceptT () m) a n
addGuard lim amt = assert (withInput lim atMost) *> addOp amt
