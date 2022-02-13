module UCapTests (testUCap) where

import UCap.Domain

import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

testUCap = testGroup "UCap domain tests"
  [testInt]
  -- [testCounter
  -- ,testConst
  -- ,testEither
  -- ,testMap
  -- ]

testInt = testGroup "IntC"
  [testCase "mincap 1" $
     mincap (addE 3) @?= addC 3
  ,testCase "mincap 2" $
     mincap (subE 3) @?= subC 3
  ,testCase "mincap 3" $
     mincap (intE (-4)) @?= subC 4
  ]

-- testCounter = testGroup "CounterC"
--   [QC.testProperty "add-mul1" $
--      \s -> eFun (addE 8 <> mulE 3) (s :: Int) == (s + 8) * 3
--   ,QC.testProperty "add-mul2" $
--      \s -> eFun (addE 1 <> mulE 34) (s :: Int) == (s + 1) * 34
--   ,QC.testProperty "mul-add1" $
--      \s -> eFun (mulE 54 <> addE 3) (s :: Int) == (s * 54) + 3
--   ,QC.testProperty "mul-add2" $
--      \s -> eFun (mulE 1 <> addE 5) (s :: Int) == (s * 1) + 5
--   ,QC.testProperty "sub-mul" $
--      \s -> eFun (subE 9 <> mulE 3) (s :: Int) == (s - 9) * 3
--   ,QC.testProperty "sub-mul2" $
--      \s -> eFun (subE 2476 <> mulE 4) (s :: Int) == (s - 2476) * 4
--   ,QC.testProperty "mul-sub" $
--      \s -> eFun (mulE 7 <> subE 34) (s :: Int) == (s * 7) - 34
--   ,testCase "simple" $
--      ((mincap (addE 3) :: IntC)
--       <=? (mincap (addE 1) <> mincap (addE 2)))
--      @?= True
--   ,testCase "simple2" $
--      ((mincap (addE 3) :: IntC) <=? addC 3)
--      @?= True
--   ]

-- testConst = testGroup "ConstC"
--   [testCase "const-modify" $
--      eFun (ConstE 2 <> addE 8) 0 @?= 10
--   ,testCase "const-modify2" $
--      eFun (ConstE (-5) <> mulE 2) 3 @?= (-10)
--   ,testCase "modify-const" $
--      eFun (addE 2 <> ConstE 74367) 4 @?= 74367
--   ,testCase "modify-const2" $
--      eFun (mulE 7 <> ConstE 0) 2 @?= 0
--   ]

-- testEither = testGroup "EitherC"
--   [testCase "left-right1" $
--      eFun (OverLR (addE 1) (addE 2) <> SetL 100) (Right 3) @?= Left 100
--   ,testCase "left-right2" $
--      eFun (OverLR (addE 1) idE <> OverLR idE (addE 3)) (Left 3) @?= Left 4
--   ,testCase "left-right3" $
--      eFun (OverLR (addE 1) idE <> OverLR idE (addE 3)) (Right 3) @?= Right 6
--   ,testCase "set-over" $
--      eFun (SetL 7 <> OverLR (subE 20) (mulE 3)) (Right 3) @?= Left (-13)
--   ,testCase "set-over2" $
--      eFun (SetL 7 <> OverLR (subE 20) (mulE 3)) (Left 3) @?= Left (-13)
--   ,testCase "set-over3" $
--      eFun (SetL 7 <> (SetR 3 <> OverLR (subE 20) (mulE 3))) (Left 3) @?= Right 9
--   ]

-- testMap = testGroup "MapC"
--   [testCase "adjust-delete" $
--      eFun (adjustE "a" (addE 1) <> deleteE "a") mempty @?= mempty
--   ,testCase "delete-adjust" $
--      eFun
--        (deleteE "a" <> adjustE "a" (addE 1))
--        (Map.fromList [("a",2)])
--      @?= mempty
--   ,testCase "insert-adjust" $
--      eFun (insertE "a" 3 <> adjustE "a" (mulE 2)) (Map.fromList [("a",8)])
--      @?= Map.fromList [("a",6)]
--   ]
