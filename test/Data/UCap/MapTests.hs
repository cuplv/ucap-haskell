module Data.UCap.MapTests (testMap) where

import Data.UCap.Map
import Data.UCap.UCT
import Data.UCap.UCT.Queue

import Control.Monad.Writer
import Data.Map (Map)
import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit

testMap = testGroup "MapC and MapE" [testQueue]

testQueue = testGroup "Queue"
  [ testCase "Queue1" $
      runWriter (execTrans'
                   [ enqueue "a" WRet
                   , enqueue "b" WRet
                   , enqueue "c" WRet
                   , enqueue "d" WRet
                   , dequeue WRet
                   , dequeue WRet
                   , dequeue WRet
                   ]
                   (Map.empty :: Map Int String))
      @?= (Map.fromList [(3 :: Int,"d")], ["0","1","2","3","a","b","c"])
  ]
