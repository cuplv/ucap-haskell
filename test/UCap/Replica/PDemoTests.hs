module UCap.Replica.PDemoTests (testPDemo) where

import UCap.Domain
import UCap.Op
import UCap.Replica
import UCap.Replica.Demo
import UCap.Replica.PDemo

import Control.Monad.Identity
import Control.Monad.Trans
import Data.Map (Map)
import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit

testPDemo = testGroup "PDemo"
  [loops
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
         evalPDemo m cc0 0 (act loopPD) @?= Identity ([],18,18)
     , testCase "Lazy loop" $
         evalPDemo m cc0 0 (act loopLazyPD) @?= Identity ([],10,18)
     ]
