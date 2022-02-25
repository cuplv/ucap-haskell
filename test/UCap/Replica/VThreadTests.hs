module UCap.Replica.VThreadTests (testVThread) where

import UCap.Replica.VClock
import UCap.Replica.VThread

import Test.Tasty
import Test.Tasty.HUnit

exthread :: VThread String String
exthread =
  event "c" "banana"
  . observe "c" "a"
  . event "b" "orange"
  . event "a" "world"
  . event "a" "hello"
  $ initThreads

exthread1 :: VThread String String
exthread1 = event "c" "apple" $ exthread

exthread2 :: VThread String String
exthread2 = event "a" "jupiter" $ exthread

exthread12 :: VThread String String
exthread12 = event "a" "jupiter" . event "c" "apple" $ exthread

testVThread = testGroup "VThread"
  [testCase "serialize" $
     serialize exthread
     @?= ["hello","orange","world","banana"]
  ,testCase "merge" $
     mergeThread exthread1 exthread2 @?= Right exthread12
  ,testCase "merge and serialize" $
     serialize <$> mergeThread exthread1 exthread2
     @?= Right ["hello","orange","world","banana","jupiter","apple"]
  ,testCase "eventImport" $
     eventImport "b" (tick "b" zeroClock, "green") exthread2
     @?= Right (event "b" "green" exthread2)
  ,testCase "eventImport 2" $
     eventImport "b" (tick "a" . tick "b" $ zeroClock, "green") exthread2
     @?= Left NotCausal
  ,testCase "eventImport 3" $
     eventImport "b" (zeroClock, "orange") exthread2
     @?= Right exthread2
  ,testCase "eventImport 5" $
     serialize <$> eventImport "z" (zeroClock, "pencil") exthread12
     @?= Right ["hello","orange","pencil","world","banana","jupiter","apple"]
  ,testCase "totalClock" $
     totalClock exthread12
     @?= (tickBy 3 "a"
          . tick "b"
          . tickBy 2 "c"
          $ zeroClock)
  ,testCase "updateClock" $
     updateClock "z" (tickBy 3 "a" zeroClock) exthread12
     @?= Right (observe "z" "a" exthread12)
  ,testCase "updateClock 2" $
     updateClock "c" (tick "a" zeroClock) exthread12
     @?= Left OldValues
  ,testCase "updateClock 3" $
     updateClock "c" (tickBy 2 "a" zeroClock) exthread12
     @?= Left OldValues
  ,testCase "updateClock 5" $
     updateClock "c" (getClock "c" exthread12) exthread2
     @?= Left MissingEvents
  ,testCase "updateClock 6" $
     updateClock "a" (getClock "a" exthread12) exthread2
     @?= Left OldValues
  ,testCase "updateClock 7" $
     updateClock "a" (tick "a" (getClock "a" exthread12)) exthread2
     @?= Left MissingEvents
  ,testCase "updateClock 4" $
     updateClock "c" (tickBy 3 "a" . tickBy 2 "c" $ zeroClock) exthread12
     @?= Right (observe "c" "a" exthread12)
  ,testCase "meetClock 1" $
     meetClock ["a","b"] exthread12 @?= zeroClock
  ,testCase "meetClock 2" $
     meetClock ["a","c"] exthread12 @?= (tickBy 2 "a" zeroClock)
  ,testCase "sizeVT" $
     sizeVT exthread12 @?= 6
  ]
