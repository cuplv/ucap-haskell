module UCap.Replica.VThreadTests (testVThread) where

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

testVThread = testGroup "VThread"
  [testCase "serialize" $
     serialize' simpleIdOrder exthread
     @?= ["hello","orange","world","banana"]
  ]
