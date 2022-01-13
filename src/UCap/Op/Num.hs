module UCap.Op.Num where

import UCap.Domain
import UCap.Op.Internal

import Control.Monad.Except

upperBound :: (Applicative m, Num n, Ord n) => Op (CounterC n) a m n
upperBound = query subAny

atMost :: (Monad m, Num n, Ord n) => Op (CounterC n) n m Bool
atMost = pairOp idOp upperBound *>= mapOp (\(x,s) -> x >= s)

lowerBound :: (Applicative m, Num n, Ord n) => Op (CounterC n) a m n
lowerBound = query addAny

atLeast :: (Monad m, Num n, Ord n) => Op (CounterC n) n m Bool
atLeast = pairOp idOp lowerBound *>= mapOp (\(x,s) -> x <= s)

addOp :: (Applicative m, Num n, Ord n) => n -> Op (CounterC n) a m n
addOp n = effect' (addE n) n

addOp' :: (Applicative m, Num n, Ord n) => Op (CounterC n) n m n
addOp' = Op uniC addAny idC $ \n -> OpBody $ \_ -> pure (addE n, n)

subOp :: (Applicative m, Num n, Ord n) => n -> Op (CounterC n) a m n
subOp n = effect' (subE n) n

subOp' :: (Applicative m, Num n, Ord n) => Op (CounterC n) n m ()
subOp' = Op uniC subAny idC $ \n -> OpBody $ \_ -> pure (subE n, ())

subGuard
  :: (Monad m, Num n, Ord n)
  => n
  -> n
  -> Op (CounterC n) a (ExceptT () m) n
subGuard lim amt = assert (withInput lim atLeast) *> subOp amt

addGuard
  :: (Monad m, Num n, Ord n)
  => n
  -> n
  -> Op (CounterC n) a (ExceptT () m) n
addGuard lim amt = assert (withInput lim atMost) *> addOp amt
