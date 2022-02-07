{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module UCap.Replica.PDemoTests (testPDemo) where

import UCap.Domain
import UCap.Lens
import UCap.Op
import UCap.Replica
import UCap.Replica.Coord
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
  ,requests
  ,tmany
  ]

type IntOp = Op (CounterC Int) Identity ()

alpha = "alpha"
beta = "beta"

-- | A looping script that pops transactions on an 'Int' state from
-- the given queue, and executes them.
intModder
  :: (Ord i)
  => Lens' s [IntOp ()]
  -> PScript i (CounterC Int) (State s)
intModder l = do
  t <- await $ popQ l
  transactSimple $ liftOpM t
  intModder l

intModder2
  :: (Ord i)
  => Lens' s [IntOp ()]
  -> Lens' s [IntOp ()]
  -> PScript i (CounterC Int) (State s)
intModder2 l1 l2 = do
  t <- await . firstOf $ [popQ l1, popQ l2]
  transactSimple $ liftOpM t
  intModder2 l1 l2

abUI :: (Cap c) => Capconf String c
abUI = capsFromList [(alpha,uniC),(beta,idC)] 

abUU :: (Cap c) => Capconf String c
abUU = capsFromList [(alpha,uniC),(beta,uniC)] 

tqueues = testGroup "Transaction queues" $
  let act1 :: PDemo String (CounterC Int) (State [IntOp ()]) (Int,Int,Int)
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
      -- cc0 = mkUniform (uniC :: CounterC Int) [alpha,beta]
      cc0 = abUU :: Capconf String (CounterC Int)
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
        evalState (evalPDemo m1 cc0 mempty 0 act1) [] @?= (3,3,18)
     ,testCase "double queue" $
        evalState (evalPDemo m2 cc0 mempty 0 act2) ([],[]) @?= (0,7,8)
     ,testCase "evalRep" $
        evalState (evalPDemo m2 cc0 mempty 0 act3) ([],[]) @?= 7
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
      cc0 = abUU
  in [ testCase "Sequential loop" $
         evalPDemo m cc0 mempty 0 (act loopSeqPD) @?= Identity ([],18,18)
     , testCase "Lazy loop" $
         evalPDemo m cc0 mempty 0 (act loopPD) @?= Identity ([],10,18)
     ]

requests = testGroup "Requests" $
  [testCase "1 Request" $
     evalPDemo (Map.fromList [(alpha, await grantLock)
                             ,(beta, do ac <- acquireLock
                                        await ac)])
               (abUI :: Capconf String (CounterC Int))
               (withLock alpha)
               0
               (do loopPD
                   rst <- lift get
                   return (rst ^. coordL beta, rst ^. capsL beta))
     @?= Identity (fst . grantReq alpha . requestLock beta $ withLock alpha
                  ,acceptG beta
                   . mdropG alpha idC
                   . fromJust . transferG alpha (beta,uniC)
                   $ abUI)
  ]

type MyC = (CounterC Int, ConstC' (IdentityC [Int]))

op1 :: Int -> Op MyC Identity () ()
op1 n = _1ed ^# (effect (addE n) :: Op (CounterC Int) Identity () ())

op2 :: [Int] -> Op (ConstC' (IdentityC [Int])) Identity () ()
op2 rs = effect (ConstE rs)

s1 :: ScriptT String MyC Identity ()
s1 = do
   rs <- transactMany $ map (\n -> op1 n >>> pure n) [1..5]
   transactMany_ [_2ed ^# op2 rs]
   loopBlock grantLock

s2 :: ScriptT String MyC Identity ()
s2 = do
  transactMany_ $ map op1 [100,200,300,400,500]
  loopBlock grantLock

tmany = testGroup "transactMany" $
  [testCase "Single no locks" $
     evalPDemo (Map.fromList [(alpha,s1)])
               abUI
               mempty
               (0,[])
               (loopPD >> lift (stateD alpha))
     @?= Identity (0 + 1 + 2 + 3 + 4 + 5, [1,2,3,4,5])
  ,testCase "Single with locks" $
     evalPDemo (Map.fromList [(alpha,s1)])
               abUI
               (withLock alpha)
               (0,[])
               (loopPD >> lift (stateD alpha))
     @?= Identity (0 + 1 + 2 + 3 + 4 + 5, [1,2,3,4,5])
  ,testCase "Two with locks" $
     evalPDemo (Map.fromList [(alpha,s1),(beta,s2)])
               abUI
               (withLock alpha)
               (0,[])
               (loopPD >> broadcast beta >> lift (stateD alpha))
     @?= Identity (0 + 1 + 2 + 3 + 4 + 5 + 100 + 200 + 300 + 400 + 500, [1,2,3,4,5])
  ]

misc = testGroup "Misc" $
  [testCase "Split CounterC" $
     split uniC uniC @?= Right (uniC :: CounterC Int)
  ,testCase "Split CounterC 2" $
     split uniC (addC 5) @?= Right (uniC :: CounterC Int)
  ,testCase "Order CounterC" $
     (uniC :: CounterC Int) <=? uniC @?= True
  ]
