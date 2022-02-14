module Data.SRQueueTests (testSRQueue) where

import Data.SRQueue

import Data.Maybe (fromJust)
import Test.Tasty
import Test.Tasty.HUnit

testSRQueue = testGroup "CRDTs"
  [testQueue
  ,testCell
  ]

testQueue = testGroup "SRQueue"
  [testCase "dequeue" $
     snd <$> srDequeue (srInit [8,4]) @?= Just 8
  ,testCase "dequeue 2" $
     (snd <$> (srDequeue . srEnqueue 4 . srEnqueue 8) srEmpty)
     @?= Just 8
  ,testCase "dequeueAll" $
     (snd . srDequeueAll . srEnqueueAll [1,2,3,4] $ srEmpty)
     @?= [1,2,3,4]
  ,testCase "Concurrent enqueue" $
     let q0 = srInit [8,4]
         q1 = srEnqueueAll [3,4,3] q0
         q2 = srEnqueue 1 q0
     in (snd . srDequeueAll $ q1 <> q2)
        @?= [8,4,1,3,4,3]
  ,testCase "Concurrent enqueue 2" $
     let q0 = srInit [8,4]
         q1 = fst . fromJust . srDequeue . srEnqueueAll [3,4,3] $ q0
         q2 = srEnqueue 1 q0
     in (snd . srDequeueAll $ q1 <> q2)
        @?= [4,1,3,4,3]
  ]

testCell = testGroup "SECell"
  [testCase "Same seSet" $
     seSet 3 (seInit 3) @?= seInit 3
  ,testCase "Same seMod" $
     seMod id (seInit 3) @?= seInit 3
  ,testCase "Merge" $
     let c0 = seInit 3
         c1 = seSet 9 c0
         c2 = seSet 1 c1
     in c2 <> c0 <> c1 @?= c2
  ]
