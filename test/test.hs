module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.UCap.InfMapTests (testInfMap)
import Data.UCap.MapTests (testMap)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" 
  [ testInfMap
  , testMap
  ]
