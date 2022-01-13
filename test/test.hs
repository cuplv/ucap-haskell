module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.InfMapTests (testInfMap)
import UCapTests (testUCap)
import UCap.MapTests (testMap)
import UCap.OpTests (testOp)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" 
  [ testInfMap
  , testUCap
  , testMap
  , testOp
  ]
