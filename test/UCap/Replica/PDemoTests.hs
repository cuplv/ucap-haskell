{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module UCap.Replica.PDemoTests (testPDemo) where

import UCap.Coord
import UCap.Domain
import UCap.Lens
import UCap.Op
import UCap.Replica
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
  ,tmany
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
-- abUI = capsFromList [(alpha,uniC),(beta,idC)] 

abIU :: (Cap c) => TokenG String c
abIU = mkTokenG beta
-- abIU = capsFromList [(alpha,idC),(beta,uniC)] 

abUU :: (Cap c) => UniversalG String c
abUU = UniversalG
-- abUU = capsFromList [(alpha,uniC),(beta,uniC)] 

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

-- requests = testGroup "Requests" $
--   [testCase "1 Request" $
--      evalPDemo (Map.fromList [(alpha, await grantLock)
--                              ,(beta, do ac <- acquireLock
--                                         await ac)])
--                (abUI :: TokenG String IntC)
--                -- (withLock alpha)
--                0
--                (do loopPD
--                    rst <- lift get
--                    return (rst ^. coordL beta, rst ^. capsL beta))
--      @?= Identity (fst . grantReq alpha . requestLock beta $ withLock alpha
--                   ,acceptG beta
--                    . mdropG alpha idC
--                    . fromJust . transferG alpha (beta,uniC)
--                    $ abUI)
--   ]

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
  ,testCase "Two with locks" $
     evalPDemo (Map.fromList [(alpha,s1),(beta,s2)])
               (abUI :: (TokenG String (IntC, ConstC (IdentityC [Int]) [Int])))
               (0,[])
               (loopPD >> broadcast beta >> lift (stateD alpha))
     @?= Identity (0 + 1 + 2 + 3 + 4 + 5 + 100 + 200 + 300 + 400 + 500, [1,2,3,4,5])
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
  -- ,testCase "dropG" $
  --    capsFlatten <$> (dropG alpha uniC (abUI :: TokenG String TestC))
  --    @?= capsFlatten <$> (Just abUI)
  -- ,testCase "dropG 2" $
  --    capsFlatten <$> dropG alpha (mincap (idE, ConstE [1]))
  --                                (abUI :: TokenG String TestC)
  --    @?= capsFlatten <$> Just abUI
  -- ,testCase "transferG" $
  --    capsFlatten <$> transferG alpha (beta,uniC :: TestC) abUI
  --    @?= capsFlatten <$> (Just abUU)
  -- ,testCase "transferG 2" $
  --    capsFlatten <$> (mdropG alpha idC <$> transferG alpha (beta,uniC :: TestC) abUI)
  --    @?= capsFlatten <$> (Just abIU)
  -- ,testCase "transferG 3" $
  --    do let cc1 = mdropG alpha idC <$> transferG alpha (beta,uniC :: TestC) abUI
  --       -- print cc1
  --       let cc2 = mdropG beta idC <$> (transferG beta (alpha, uniC) =<< acceptG beta <$> cc1)
  --       capsFlatten <$> cc2 @?= capsFlatten <$> Just abUI
  -- ,testCase "transferG 3.1" $
  --    capsFlatten <$> ((\cc -> mdropG beta idC <$> transferG beta (alpha, uniC) cc) =<< acceptG beta <$> (mdropG alpha idC <$> transferG alpha (beta,uniC :: TestC) abUI))
  --    @?= capsFlatten <$> (Just abUI)
  -- ,testCase "transferG 4" $
  --    capsFlatten <$> transferG beta (alpha, uniC :: TestC) abIU
  --    @?= capsFlatten <$> (Just abUU)
  ]
