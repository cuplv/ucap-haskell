module UCap.OpTests (testOp) where

import UCap.Domain
import UCap.Lens
import UCap.Op

import Data.Functor.Identity (Identity)
import Test.Tasty
import Test.Tasty.HUnit

testOp = testGroup "Operations" [generic]

pure' = pure :: a -> Identity a

generic = testGroup "Generic"
  [testCase "mapOp" $
     evalOp (mapOp (+ 1) & withInput 1) @?= pure' 2
  ,testCase "mapOp3" $
     evalOp (mapOp (++ "b") `pipe` mapOp (++ "c") & withInput "a")
     @?= pure' "abc"
  ,testCase "effect" $
     execOp 1 (query (uniC::IntC) >>> effect (addE 1))
     @?= pure' (2,1)
  ,testCase "effect2" $
     execOp 1 (effect (addE 1) *> query (uniC::IntC))
     @?= pure' (2,2)
  ,testCase "pair" $
     evalOp (pairOp (mapOp (++ "b")) (mapOp (++ "c")) & withInput "a")
     @?= pure' ("ab","ac")
  ,testCase "pair2" $
     let o1 = pairOp
                (query (uniC::IntC) >>> effect (addE 1))
                (mapOp (++ "b"))
         o2 = pairOp
                (query uniC >>> effect (addE 2))
                (mapOp (++ "c"))
     in execOp (1::Int) (pairOp o1 o2 & withInput "a")
        @?= pure' (4, ((1,"ab"),(2,"ac")))
  ]
