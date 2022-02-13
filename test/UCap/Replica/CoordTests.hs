module UCap.Replica.CoordTests (testCoord) where

import UCap.Domain
-- import UCap.Replica.Capconf
-- import UCap.Replica.Coord

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Test.Tasty
import Test.Tasty.HUnit

testCoord = testGroup "Replica.Coord" []

-- testCoord = testGroup "Coord"
--   [testCaps
--   ,testLocks
--   ]

-- alpha = "alpha"
-- beta = "beta"

-- abUI :: Capconf String (CounterC Int)
-- abUI = capsFromList [(alpha,uniC),(beta,idC)]

-- abIU :: Capconf String (CounterC Int)
-- abIU = capsFromList [(alpha,idC),(beta,uniC)]

-- abUU :: Capconf String (CounterC Int)
-- abUU = capsFromList [(alpha,uniC),(beta,uniC)]

-- testCaps = testGroup "Capconf"
--   [testCase "Flatten 1" $
--      capsFlatten abUI @?= Map.fromList [(alpha,uniC),(beta,idC)]
--   ,testCase "Flatten 2" $
--      capsFlatten abUU @?= Map.fromList [(alpha,uniC),(beta,uniC)]
--   ,testCase "Flatten 3" $
--      capsFlatten abIU @?= Map.fromList [(alpha,idC),(beta,uniC)]
--   ,testCase "Transfer and drop" $
--      capsFlatten 
--        (acceptG beta
--         . mdropG alpha idC
--         . fromJust . transferG alpha (beta,uniC)
--         $ abUI)
--      @?= capsFlatten abIU
--   ]

-- testLocks = testGroup "Locks" $
--   let cd0 = withLock "a" :: Coord String (IdentityC ())
--   in [testCase "Trivial isRequestedOf" $
--         (isRequestedOf "a" $ requestLock "b" cd0) @?= True
--      ,testCase "Request isRequestedOf" $
--         (isRequestedOf "a" $ cd0 <> requestLock "b" cd0 <> cd0)
--         @?= True
--      ,testCase "Request isOnwer" $
--         (ownsLock "a" $ cd0 <> requestLock "b" cd0 <> cd0)
--         @?= True
--      ,testCase "Granted" $
--         let cd1 = requestLock "b" $ withLock "a" :: Coord String (IdentityC ())
--             cd2 = cd1 <> fst (grantReq "a" cd1) <> cd1
--         in (ownsLock "a" cd2, ownsLock "b" cd2)
--            @?= (False,True)
--      ]
