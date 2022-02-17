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
  ,testCase "duplication" $
     let q1 = mwEnqueue "a" 4 mwEmpty
     in q1 <> q1 @?= q1
  ,testCase "duplication 2" $
     let q1 = mwEnqueue "a" 4 mwEmpty
         q2 = mwEnqueue "a" 9 q1
     in q1 <> q2 @?= q2
  ,testCase "duplication 3" $
     let q1 = mwEnqueue "a" 4 mwEmpty
         q2 = fst . fromJust $ mwDequeue (mwEnqueue "a" 9 q1)
     in q1 <> q2 @?= q2
  ,testCase "length 1" $
     let q1 = mwEmpty :: MWQueue String Int
         q2 = mwEnqueue "a" 1 . mwEnqueue "b" 8 . mwEnqueue "a" 4 $ q1
         q3 = fst $ mwDequeueAll q2
     in (mwLength q1, mwLength q2, mwLength q3)
        @?= (0,3,0)
  ,testCase "merge 0" $
     let q1 = srEnqueue "beta" srEmpty
         q2 = fst <$> srDequeue q1
     in (q1 <>) <$> q2 @?= q2
  ,testCase "merge 1" $
     let q2 = mwEnqueue "beta" "beta" mwEmpty
         q3 = fst <$> mwDequeue q2
     in (q2 <>) <$> q3 @?= q3
  ,testCase "peek" $
     (srPeek . srEnqueue 4 . srEnqueue 8 $ srEmpty) @?= Just 8
  ,testCase "peek mw 1" $
     (mwPeek . fst . fromJust . mwDequeue . mwEnqueue "a" 8 $ mwEmpty)
     @?= Nothing
  ,testCase "peek mw 2" $
     (mwPeek . fst . fromJust . mwDequeue . mwEnqueueAll "a" [8,1] $ mwEmpty)
     @?= Just 1
  ,testCase "dequeueAll mw" $
     (snd . mwDequeueAll
      . mwEnqueueAll "a" [1,2,3,4]
      . mwEnqueueAll "b" [8,9]
      $ mwEmpty)
     @?= [1,2,3,8,4,9]
  ,testCase "peekAll" $
     (srPeekAll . srEnqueue 4 . srEnqueue 8 $ srEmpty) @?= [8,4]
  ,testCase "peekAll mw" $
     (mwPeekAll . mwEnqueue "b" 4 . mwEnqueue "a" 8 $ mwEmpty) @?= [8,4]
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
  ,testCase "Concurrent enqueue 3" $
     let q0 = srInit [8,4]
         q1 = fst . fromJust . srDequeue . srEnqueueAll [3,4,3] $ q0
         q2 = srEnqueue 1 q0
         q3 = fst . srDequeueAll $ q2 <> q1
         q4 = srEnqueue 56 q3
         q5 = srEnqueue 638 q3
     in (snd . srDequeueAll $ q5 <> q4)
        @?= [56,638]
  ,testCase "Concurrent enqueue 4" $
     let q = srInit [1,2]
         q3 = srEnqueueAll [3,0,5] q
         q4 = srEnqueue 4 q
     in srPeekAll (q3 <> q4) @?= [1,2,3,0,4,5]
  ,testCase "Concurrent enqueue 5" $
     let q = srInit [1,2]
         q3 = srEnqueueAll [3,0,5] q
         q4 = srEnqueue 4 q
     in srPeekAll (q4 <> q3) @?= [1,2,3,0,4,5]
  ,testCase "Concurrent enqueue 6" $
     let q = mwEnqueue "c" 9 $ mwEmpty
         q1 = mwEnqueue "a" 1 q
         q1' = fst . fromJust $ mwDequeue q1
         q2 = mwEnqueue "b" 2 q
     in mwPeekAll (q1' <> q2) @?= [2,9]
  ,testCase "Concurrent enqueue 6" $
     let q = mwEnqueue "c" 9 $ mwEmpty
         q1 = mwEnqueue "a" 1 q
         q1' = fst . fromJust $ mwDequeue q1
         q2 = mwEnqueue "b" 2 q
     in mwPeekAll (q2 <> q1') @?= [2,9]
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
