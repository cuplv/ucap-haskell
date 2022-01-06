module Data.UCap.Op.Counter where

import Data.UCap
import Data.UCap.Op.Internal

atLeast :: (Num n, Ord n) => n -> Op' (CounterC n) ()
atLeast n = opTest
  addAny
  (>= n)

atMost :: (Num n, Ord n) => n -> Op' (CounterC n) ()
atMost n = opTest
  subAny
  (<= n)

subOp :: (Num n, Ord n) => n -> Op' (CounterC n) n
subOp amt = opEffect (subE amt) *> pure (pure amt)

preSub :: (Num n, Ord n) => PreOp (CounterC n) n n
preSub = mkPre uniC subAny $ \n -> opBEffect (subE n) >> return n

subGuard :: (Num n, Ord n) => n -> n -> Op' (CounterC n) n
subGuard lim amt = atLeast lim *> subOp amt

addOp :: (Num n, Ord n) => n -> Op' (CounterC n) n
addOp amt = opEffect (addE amt) *> pure (pure amt)

preAdd :: (Num n, Ord n) => PreOp (CounterC n) n n
preAdd = mkPre uniC addAny $ \n -> opBEffect (addE n) >> return n
