{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module UCap.Replica.PDemoTests (testPDemo) where

import UCap.Coord
import UCap.Domain
import UCap.Lens
import UCap.Op
import UCap.Replica
import UCap.Replica.Transact
import UCap.Replica.Demo
import UCap.Replica.PDemo

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Test.Tasty
import Test.Tasty.HUnit

testPDemo = testGroup "PDemo"
  [misc
  ,loops
  ,tqueues
  ,ttoken
  ,tmany
  ,tescrow
  ]

type IntOp = Op IntC Identity ()

alpha = "alpha"
beta = "beta"

-- | A looping script that pops transactions on an 'Int' state from
-- the given queue, and executes them.
intModder
  :: (Ord i)
  => Lens' s [IntOp ()]
  -> PScript (UniversalG i IntC) (State s)
intModder l = do
  t <- await $ popQ l
  transactSimple $ liftOpM t
  intModder l

intModder2
  :: (Ord i)
  => Lens' s [IntOp ()]
  -> Lens' s [IntOp ()]
  -> PScript (UniversalG i IntC) (State s)
intModder2 l1 l2 = do
  t <- await . firstOf $ [popQ l1, popQ l2]
  transactSimple $ liftOpM t
  intModder2 l1 l2

abUI :: TokenG String c
abUI = mkTokenG alpha

abIU :: (Cap c) => TokenG String c
abIU = mkTokenG beta

abUU :: (Cap c) => UniversalG String c
abUU = UniversalG

ttoken = testGroup "TokenG operations" $
  [testCase "Request" $
     isRequestedOf alpha (requestToken beta (mkToken alpha))
     @?= True
  ,testCase "Request 2" $
     isRequestedOf beta (requestToken beta (mkToken alpha))
     @?= False
  ,testCase "Request 3" $
     isRequestedOf alpha <$> (grantToken alpha
                              . requestToken beta $ mkToken alpha)
     @?= Right False
  ,testCase "Request 4" $
     tokenOwner <$> (grantToken alpha
                     . requestToken beta $ mkToken alpha)
     @?= Right beta
  ,testCase "Concurrent" $
     let q1 = mkToken alpha
         q2 = requestToken beta q1
         q3 = grantToken alpha (q1 <> q2)
     in tokenOwner <$> ((q1 <>) <$> q3) @?= Right beta
  ,testCase "Concurrent 2" $
     let q1 = mkToken alpha
         q2 = requestToken beta q1
         q3 = grantToken alpha (q1 <> q2)
     in ((q1 <>) <$> q3) @?= q3
  ,testCase "Concurrent 2" $
     let q1 = mkToken alpha
         q2 = requestToken beta q1
         q3 = grantToken alpha (q1 <> q2)
     in ((q2 <>) <$> q3) @?= q3
  ]

tqueues = testGroup "Transaction queues" $
  let act1 :: PDemo (UniversalG String IntC) (State [IntOp ()]) (Int,Int,Int)
      act1 = do
        liftPDemo $ modify (++ [effect (addE 3)])
        loopPD
        s1 <- lift $ stateD alpha
        loopPD
        s2 <- lift $ stateD alpha
        liftPDemo $ modify (++ [effect (addE 6), effect (addE 9)])
        loopPD
        s3 <- lift $ stateD alpha
        return (s1,s2,s3)
      g0 = abUU :: UniversalG String IntC
      m1 = Map.singleton alpha (intModder id)
      act2 = do
        loopPD
        liftPDemo $ _1 %= (++ [effect (addE 3)])
        liftPDemo $ _2 %= (++ [effect (addE 4)])
        s1 <- lift $ stateD alpha
        loopPD
        s2 <- lift $ stateD alpha
        liftPDemo $ _2 %= (++ [effect (addE 1)])
        loopPD
        s3 <- lift $ stateD alpha
        return (s1,s2,s3)
      m2 = Map.singleton alpha (intModder2 _1 _2)
      act3 = do
        liftPDemo $ _1 %= (++ [effect (addE 3)])
        liftPDemo $ _2 %= (++ [effect (addE 4)])
        evalRep alpha
        lift $ stateD alpha
  in [testCase "single queue" $
        evalState (evalPDemo m1 g0 0 act1) [] @?= (3,3,18)
     ,testCase "double queue" $
        evalState (evalPDemo m2 g0 0 act2) ([],[]) @?= (0,7,8)
     ,testCase "evalRep" $
        evalState (evalPDemo m2 g0 0 act3) ([],[]) @?= 7
     ]

loops = testGroup "Loops" $
  let s1 = do
        transactSimple (effect $ addE 5)
        transactSimple (query uniC >>> addOp')
        return ()
      s2 = transactSimple (effect $ addE 8) >> return ()
      m = Map.fromList [("alpha", s1), ("beta", s2)]
      act m = do
        r <- m
        s1 <- lift $ stateD "alpha"
        broadcast "beta"
        s2 <- lift $ stateD "alpha"
        return (r,s1,s2)
      g0 = abUU
  in [ testCase "Sequential loop" $
         evalPDemo m g0 0 (act loopSeqPD) @?= Identity ([],18,18)
     , testCase "Lazy loop" $
         evalPDemo m g0 0 (act loopPD) @?= Identity ([],10,18)
     ]

type MyC = (IntC, ConstC' (IdentityC [Int]))

type MyG = TokenG String MyC

op1 :: Int -> Op MyC Identity () ()
op1 n = _1ed ^# (effect (addE n) :: Op IntC Identity () ())

op2 :: [Int] -> Op (ConstC' (IdentityC [Int])) Identity () ()
op2 rs = effect (ConstE rs)

s1 :: ScriptT MyG Identity ()
s1 = do
   rs <- transactMany $ map (\n -> op1 n >>> pure n) [1..5]
   transactMany_ [_2ed ^# op2 rs]
   loopBlock grantRequests'

s2 :: ScriptT MyG Identity ()
s2 = do
  transactMany_ $ map op1 [100,200,300,400,500]
  loopBlock grantRequests'

tmany = testGroup "transactMany" $
  [testCase "Single no locks" $
     evalPDemo (Map.fromList [(alpha,s1)])
               abUI
               (0,[])
               (loopPD >> lift (stateD alpha))
     @?= Identity (0 + 1 + 2 + 3 + 4 + 5, [1,2,3,4,5])
  ,testCase "Single with locks" $
     evalPDemo (Map.fromList [(alpha,s1)])
               abUI
               (0,[])
               (loopPD >> lift (stateD alpha))
     @?= Identity (0 + 1 + 2 + 3 + 4 + 5, [1,2,3,4,5])
  ,testCase "Single transactor" $
     evalPDemo (Map.fromList [(alpha,s1),(beta, loopBlock grantRequests')])
               abIU
               (0,[])
               (loopPD >> lift (stateD alpha))
     @?= Identity (0 + 1 + 2 + 3 + 4 + 5, [1,2,3,4,5])
  ,testCase "Two with locks" $
     evalPDemo (Map.fromList [(alpha,s1),(beta,s2)])
               (abUI :: TokenG String (IntC, ConstC (IdentityC [Int]) [Int]))
               (0,[])
               (loopPD >> broadcast beta >> lift (stateD alpha))
     @?= Identity ( 0 + 1 + 2 + 3 + 4 + 5 + 100 + 200 + 300 + 400 + 500
                  , [1,2,3,4,5] )
  ]

type TestC = (IntC, ConstC (IdentityC [Int]) [Int])

misc = testGroup "Misc" $
  [testCase "Split CounterC" $
     split uniC uniC @?= Right (uniC :: IntC)
  ,testCase "Split CounterC 2" $
     split uniC (addC 5) @?= Right (uniC :: IntC)
  ,testCase "Split Pair" $
     split uniC (addC 5, idC)
     @?= Right (uniC :: TestC)
  ,testCase "Split ID" $
     split (uniC :: IdentityC [Int]) uniC @?= Right uniC
  ,testCase "Split CID" $
     split (uniC :: ConstC (IdentityC [Int]) [Int]) uniC @?= Right uniC
  ,testCase "Order CounterC" $
     (uniC :: IntC) <=? uniC @?= True
  ]

escSys1 :: IntEscrow String
escSys1 = initIntEscrow 1 ["A"] $ Map.fromList
  [("A",(0,102))
  ,("B",(0,3))
  ,("C",(0,2))
  ]

escSys2 :: IntEscrow String
escSys2 = initIntEscrow 1 ["A"] $ Map.fromList
  [("A",(0,102))
  ,("B",(0,3))
  ,("C",(0,0))
  ]

escSys3 :: IntEscrow String
escSys3 = initIntEscrow 1 ["A"] $ Map.fromList
  [("A",(0,102))
  ,("B",(0,3))
  ]

esc1 :: ScriptT (IntEscrow String) Identity ()
esc1 = loopBlock grantRequests'

esc2 :: ScriptT (IntEscrow String) Identity ()
esc2 = do
  rs <- transactMany . repeat $ effect (addE 1)
  loopBlock grantRequests'

tescrow = testGroup "Escrow" $
  [testCase "Escrow 1.1" $
     -- Replicas B and C should be able to use all of A's resources,
     -- but no more, in their transactions before they get stuck.
     evalPDemo
       (Map.fromList [("A",esc1)
                     ,("B",esc2)
                     ,("C",esc2)
                     ])
       escSys1
       0
       (loopPD >> lift (stateD "A"))
     @?= Identity 107
  ,testCase "Escrow 1.2" $
     evalPDemo
       (Map.fromList [("A",esc1)
                     ,("B",esc2)
                     ,("C",esc2)
                     ])
       escSys2
       0
       (loopPD >> lift (stateD "A"))
     @?= Identity 105
  ,testCase "Escrow 1.3" $
     evalPDemo
       (Map.fromList [("A",esc1)
                     ,("B",esc2)
                     ,("C",esc2)
                     ])
       escSys3
       0
       (loopPD >> lift (stateD "A"))
     @?= Identity 105
  ,testCase "Escrow 2" $
     evalPDemo
       (Map.fromList [("A",esc1)
                     ,("B",esc2)
                     ])
       escSys1
       0
       (loopPD >> lift (stateD "A"))
     @?= Identity 105
  ,testCase "Escrow accept" $
     let e = initEscrow ["A"] []
               (Map.fromList [("A",102), ("B",3), ("C",2)])
     in escrowOwned "B" . escrowAccept "B" <$> escrowTransfer "A" ("B",70) e
        @?= Right 73
  ,testCase "Escrow unowned" $
     let e = initEscrow [] [] $ Map.fromList [("A",60), ("B",3), ("C", 1)]
     in escrowUnowned "B" <$> escrowTransfer "A" ("B",20) e
        @?= Right 61
  ,testCase "Escrow unowned 2" $
     let e = initEscrow [] [] $ Map.fromList [("A",60), ("B",3), ("C", 1)]
     in escrowUnowned "C" <$> escrowTransfer "A" ("B",20) e
        @?= Right 63
  ,testCase "Escrow accept 2" $
     let e1 = initEscrow ["A"] []
                (Map.fromList [("A",102), ("B",3), ("C",2)])
         e2 = initEscrow ["A"] []
                (Map.fromList [("A",32), ("B",73), ("C",2)])
     in escrowAccept "B" e1 @?= e1
  ,testCase "Escrow read" $
     let g :: IntEscrow String
         g = initIntEscrow 1 [] $ Map.fromList
                                    [("A",(subA,0)),("B",(subB,0))]
         subA = 8
         subB = 46
         s0 = 4
     in evalDemo ["A","B"] g s0 (noBlock $ "A" .// lowerBound)
        @?= Identity (Right $ s0 - subB)
  ,testCase "Escrow concurrent requests" $
     let g0 = initEscrow ["A"] [] $ Map.fromList
                [("A",52)
                ]
         g1 = escrowRequest "B" ("A",1) g0
         g2 = escrowRequest "C" ("A",3) g0
         g' = escrowAccept "B" . escrowAccept "C" $
                fromJust . escrowHandleReqs "A" $
                  fromJust . escrowHandleReqs "A" $ (g1 <> g2)
     in (escrowOwned "B" g', escrowOwned "C" g') @?= (1,3)
  ,testCase "Escrow concurrent requests 2" $
     let g0 = initEscrow ["A"] [] $ Map.fromList
                [("A",52)
                ]
         g1 = escrowRequest "B" ("A",1) g0
         g2 = escrowRequest "C" ("A",3) g0
         g' = escrowAccept "B" . escrowAccept "C" $
                fromJust . escrowHandleReqs "A" $
                  fromJust . escrowHandleReqs "A" $ (g2 <> g1)
     in (escrowOwned "B" g', escrowOwned "C" g') @?= (1,3)
  ,testCase "Escrow concurrent requests 3" $
     let g0 = initEscrow ["A"] [] $ Map.fromList
                [("A",52)
                ]
         g1 = escrowRequest "B" ("A",1) g0
         g2 = fromJust . escrowHandleReqs "A" $ escrowRequest "C" ("A",3) g0
         g' = escrowAccept "B" $
                fromJust . escrowHandleReqs "A" $ (g2 <> g1)
     in (escrowOwned "B" g') @?= 1
  ]
