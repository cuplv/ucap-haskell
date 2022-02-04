module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.InfMapTests (testInfMap)
import UCapTests (testUCap)
import UCap.MapTests (testMap)
import UCap.OpTests (testOp)
import UCap.Replica.CoordTests (testCoord)
import UCap.Replica.DemoTests (testDemo)
import UCap.Replica.PDemoTests (testPDemo)
import UCap.Replica.VClockTests (testVClock)
import UCap.Replica.VThreadTests (testVThread)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" 
  [ testInfMap
  , testVClock
  , testUCap
  , testMap
  , testOp
  , testVThread
  , testCoord
  , testDemo
  , testPDemo
  ]
