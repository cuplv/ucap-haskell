module Data.UCap.Op.Counter where

import Data.UCap
import Data.UCap.Op.Internal

import Control.Monad.Except

upperBound :: (Applicative m, Num n, Ord n) => Op (CounterC n) a m n
upperBound = queryOp subAny

atMost :: (Monad m, Num n, Ord n) => Op (CounterC n) n m Bool
atMost = pairOp idOp upperBound *>= mapOp (\(x,s) -> x >= s)

lowerBound :: (Applicative m, Num n, Ord n) => Op (CounterC n) a m n
lowerBound = queryOp addAny

atLeast :: (Monad m, Num n, Ord n) => Op (CounterC n) n m Bool
atLeast = pairOp idOp lowerBound *>= mapOp (\(x,s) -> x <= s)

addOp :: (Applicative m, Num n, Ord n) => n -> Op (CounterC n) a m n
addOp n = effectOp' (addE n) n

addOp' :: (Applicative m, Num n, Ord n) => Op (CounterC n) n m n
addOp' = Op uniC addAny idC $ \n -> OpBody $ \_ -> pure (addE n, n)

subOp :: (Applicative m, Num n, Ord n) => n -> Op (CounterC n) a m n
subOp n = effectOp' (subE n) n

subOp' :: (Applicative m, Num n, Ord n) => Op (CounterC n) n m ()
subOp' = Op uniC subAny idC $ \n -> OpBody $ \_ -> pure (subE n, ())

subGuard
  :: (Monad m, Num n, Ord n)
  => n
  -> n
  -> Op (CounterC n) a (ExceptT () m) n
subGuard lim amt = testOp (lim `feedTo` atLeast) *> subOp amt

addGuard
  :: (Monad m, Num n, Ord n)
  => n
  -> n
  -> Op (CounterC n) a (ExceptT () m) n
addGuard lim amt = testOp (lim `feedTo` atMost) *> addOp amt
