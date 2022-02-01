module UCap.Replica.DemoTests (testDemo) where

import UCap
import UCap.Op
import UCap.Replica.Demo

import Control.Monad.Identity
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
  ,testCase "evalU 2" $
     let script = do
           r <- transactD "alpha" (lowerBound >>> mapOp (+ 1) >>> addOp')
           s <- stateD "alpha"
           return (r,s)
     in evalDemoU ["alpha","beta"] uniC 0 script
        @?= Identity (Right ((Nothing, 0)))
  ]
