module UCap.MapTests (testMap) where

import UCap.Domain

import Control.Monad.Writer
import Data.Map (Map)
import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit

testMap = testGroup "MapC and MapE" [testQueue]

testQueue = testGroup "Queue"
  [testCase "MapC split" $
     split (uniC :: (SetC Int, SetC Int, SetC Int)) idC @?= Right uniC
  ,testCase "MapC split 2" $
     let c = mincap (idE, idE, insertE 5 ()) :: (SetC Int, SetC Int, SetC Int)
     in split idC c @?= Left c
  ,testCase "MapC split 3" $
     let c = mincap (idE, idE, insertE 5 ()) :: (SetC Int, SetC Int, SetC Int)
     in split uniC c @?= Right uniC
  ,testCase "MapC split 5" $
     split (uniC :: (SetC Int, SetC Int, SetC Int)) uniC @?= Right uniC
  ,testCase "MapC split 6" $
     split (idC :: (SetC Int, SetC Int, SetC Int)) uniC @?= Left uniC
  ,testCase "MapC split 7" $
     split (idC :: (SetC Int, SetC Int, SetC Int)) idC @?= Right idC
  ,testCase "Tuple <=?" $
     (idC :: (IntC, IntC, IntC)) <=? idC @?= True
  ,testCase "MapC <=? 1" $
     (idC :: (SetC Int, SetC Int, SetC Int)) <=? idC @?= True
  ,testCase "MapC <=? 2" $
     (uniC :: (SetC Int, SetC Int, SetC Int)) <=? idC @?= False
  ,testCase "MapC <=? 3" $
     (idC :: (SetC Int, SetC Int, SetC Int)) <=? uniC @?= True
  ,testCase "MapC <=? 4" $
     (uniC :: (SetC Int, SetC Int, SetC Int)) <=? uniC @?= True
  ]
