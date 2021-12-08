module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.UCap.InfMapTests

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" 
  [ unitTests
  , testInfMap
  ]

unitTests = testGroup "Unit tests"
  [ testCase "List length" $ length [1,2,3] @?= 3 ]
