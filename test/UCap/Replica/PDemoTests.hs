module UCap.Replica.PDemoTests (testPDemo) where

import UCap.Domain
import UCap.Op
import UCap.Replica
import UCap.Replica.Demo
import UCap.Replica.PDemo

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans
import Data.Map (Map)
import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit

testPDemo = testGroup "PDemo"
  [loops
  ,tqueues
  ]

type IntOp = Op (CounterC Int) Identity ()

alpha = "alpha"
beta = "beta"

tqueues = testGroup "Transaction queues" $
  let rsc :: PScript String (CounterC Int) (State [IntOp ()])
      rsc = do
          t <- await [popQ id]
          transact $ liftOpM t
          rsc
      act :: PDemo String (CounterC Int) (State [IntOp ()]) (Int,Int,Int)
      act = do
        liftPDemo $ modify (++ [effect (addE 3)])
        loopPD
        s1 <- lift $ stateD alpha
        loopPD
        s2 <- lift $ stateD alpha
        liftPDemo $ modify (++ [effect (addE 6), effect (addE 9)])
        loopPD
        s3 <- lift $ stateD alpha
        return (s1,s2,s3)
      cc0 = mkUniform uniC [alpha]
      m = Map.singleton alpha rsc
  in [testCase "single queue" $
        evalState (evalPDemo m cc0 0 act) [] @?= (3,3,18)
     ]

loops = testGroup "Loops" $
  let s1 = do
        transact (effect $ addE 5)
        transact (query uniC >>> addOp')
        return ()
      s2 = transact (effect $ addE 8) >> return ()
      m = Map.fromList [("alpha", s1), ("beta", s2)]
      act m = do
        r <- m
        s1 <- lift $ stateD "alpha"
        broadcast "beta"
        s2 <- lift $ stateD "alpha"
        return (r,s1,s2)
      cc0 = mkUniform uniC ["alpha","beta"]
  in [ testCase "Sequential loop" $
         evalPDemo m cc0 0 (act loopSeqPD) @?= Identity ([],18,18)
     , testCase "Lazy loop" $
         evalPDemo m cc0 0 (act loopPD) @?= Identity ([],10,18)
     ]
