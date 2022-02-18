module UCap.Replica.DemoTests (testDemo) where

import UCap
import UCap.Coord
import UCap.Op
import UCap.Replica.Demo
import UCap.Replica.Script

import Control.Monad.Identity
import Data.Either (fromRight)
import Test.Tasty
import Test.Tasty.HUnit

testDemo = testGroup "Demo"
  [testCase "evalU S" $
     let -- m :: DemoState (UniversalG String (CounterC Int)) Identity Int
         m = do noBlock $ "alpha" .// effect (addE 5)
                noBlock $ "beta" .// effect (addE 8)
                noBlock $ "alpha" .// (query uniC >>> addOp')
                observeD "alpha" "beta"
                stateD "alpha"
     in evalDemo ["alpha","beta"] UniversalG 0 m @?= Identity 18
  ]
