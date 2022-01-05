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

preSub :: (Num n, Ord n) => PreOp (CounterC n) n n
preSub = mkPre uniC subAny $ \n -> opBEffect (subE n) >> return n

subGuard :: (Num n, Ord n) => n -> n -> Op' (CounterC n) ()
subGuard lim amt = atLeast lim *> opEffect (subE amt)
