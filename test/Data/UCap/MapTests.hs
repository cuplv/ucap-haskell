module Data.UCap.MapTests (testMap) where

import Data.UCap

import Control.Monad.Writer
import Data.Map (Map)
import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit

testMap = testGroup "MapC and MapE" [testQueue]

testQueue = testGroup "Queue" []
