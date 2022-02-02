module UCap.Replica.DemoTests (testDemo) where

import UCap
import UCap.Op
import UCap.Replica.Demo
import UCap.Replica.Script

import Control.Monad.Identity
import Data.Either (fromRight)
import Test.Tasty
import Test.Tasty.HUnit

testDemo = testGroup "Demo"
  [testCase "evalU" $
     let script = do
           transactD "alpha" (addOp 5)
           transactD "beta" (addOp 8)
           transactD "alpha" (query uniC >>> addOp')
           observeD "alpha" "beta"
           stateD "alpha"
     in evalDemoU ["alpha","beta"] uniC 0 script @?= Identity (Right 18)
  ,testCase "evalU S" $
     let m = do "alpha" ./$ transact (addOp 5)
                "beta" ./$ transact (addOp 8)
                "alpha" ./$ transact (query uniC >>> addOp')
                observeD "alpha" "beta"
                "alpha" ./$ readState
     in fromRight (-1) (evalDemoU' ["alpha","beta"] uniC 0 m) @?= 18
  ,testCase "evalU 2" $
     let script = do
           r <- transactD "alpha" (lowerBound >>> mapOp (+ 1) >>> addOp')
           s <- stateD "alpha"
           return (r,s)
     in evalDemoU ["alpha","beta"] uniC 0 script
        @?= Identity (Right ((Nothing, 0)))
  ,testCase "evalU S 2" $
     let m = do r <- "alpha" ./$ transact (lowerBound >>> mapOp (+ 1) >>> addOp')
                s <- "alpha" ./$ readState
                return (fromRight undefined r, fromRight undefined s)
     in evalDemoU' ["alpha","beta"] uniC 0 m
        @?= (Nothing,0)
  ]
