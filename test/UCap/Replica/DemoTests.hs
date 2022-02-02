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
     in evalDemoU ["alpha","beta"] uniC 0 script @?= Identity 18
  ,testCase "evalU S" $
     let m = do noBlock $ "alpha" .// effect (addE 5)
                noBlock $ "beta" .// effect (addE 8)
                noBlock $ "alpha" .// (query uniC >>> addOp')
                observeD "alpha" "beta"
                stateD "alpha"
     in evalDemoU' ["alpha","beta"] uniC 0 m @?= 18
  ,testCase "evalU 2" $
     let script = do
           r <- transactD "alpha" (lowerBound >>> mapOp (+ 1) >>> addOp')
           s <- stateD "alpha"
           return (r,s)
     in evalDemoU ["alpha","beta"] uniC 0 script
        @?= Identity (Nothing, 0)
  ,testCase "evalU S 2" $
     let m = do r <- noBlock $ "alpha" .// (lowerBound >>> mapOp (+ 1) >>> addOp')
                s <- noBlock $ "alpha" .// query uniC
                return (r, s)
     in evalDemoU' ["alpha","beta"] uniC 0 m
        @?= (Nothing, Just 0)
  ]
