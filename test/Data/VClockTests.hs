module Data.VClockTests (testVClock) where

import Data.VClock

import Test.Tasty
import Test.Tasty.HUnit

zc = zeroClock :: VClock String

testVClock = testGroup "VClock"
  [testCase "zero" $
     zc `precedes` zc @?= False
  ,testCase "zero1" $
     lookupVC "a" zc @?= 0
  ,testCase "tick" $
     zc `precedes` tick "a" zc @?= True
  ,testCase "tick1" $
     tick "z" zc `precedes` zc @?= False
  ,testCase "tick2" $
     tick "f" zc `concurrent` tick "h" zc @?= True
  ,testCase "tick3" $
     tick "f" zc == tick "h" zc @?= False
  ]
