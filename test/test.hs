module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.UCapTests (testUCap)
import Data.UCap.InfMapTests (testInfMap)
import Data.UCap.MapTests (testMap)
import Data.UCap.OpTests (testOp)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" 
  [ testInfMap
  , testUCap
  , testMap
  , testOp
  ]
