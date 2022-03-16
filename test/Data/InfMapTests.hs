module Data.InfMapTests (testInfMap) where

import UCap.Domain (meetId, (<=?))

import Data.Map (Map)
import qualified Data.Map as Map
import Data.InfMap (InfMap)
import qualified Data.InfMap as IM

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

prop_commuteInsert :: Int -> Map Int Int -> (Int,Int) -> Bool
prop_commuteInsert v0 m (k,v) =
  IM.fromMap v0 (Map.insert k v m)
  == IM.insert k v (IM.fromMap v0 m)

testInfMap = testGroup "InfMap"
  [ QC.testProperty "insert" prop_commuteInsert
  , testCase "map" $ 
      IM.lookup "a" (IM.map (+1) $ IM.uniform 3) @?= 4
  , testCase "unionWith" $
      IM.lookup "a" (IM.unionWith 
                       (+) 
                       (IM.uniform 3) 
                       (IM.fromList 2 [("a",5),("b",6)]))
      @?= 8
  , testCase "Compare" $
      (IM.uniform () :: InfMap Int ()) <=? IM.uniform () @?= True
  , testCase "Compare 2" $
      (IM.uniform () :: InfMap Int ()) <=? meetId @?= True
  , testCase "Compare 3" $
      meetId <=? (mempty :: InfMap Int ()) @?= True
  , testCase "Compare 3.1" $
      meetId == (mempty :: InfMap Int ()) @?= True
  , testCase "Compare 4" $
      (meetId :: InfMap Int ()) <=? meetId @?= True
  , testCase "Compare 5" $
      (IM.fromList () [(1,())]) <=? meetId @?= True
  ]
