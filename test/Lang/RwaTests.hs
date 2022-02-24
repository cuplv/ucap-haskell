{-# LANGUAGE TypeFamilies #-}

module Lang.RwaTests (testRwa) where

import Lang.Rwa
import Lang.Rwa.Interpret

import Control.Monad.Identity
import Control.Monad.State
import Test.Tasty
import Test.Tasty.HUnit

newtype SGState a = SGState a deriving (Show,Eq,Ord)

instance (Semigroup a) => Semigroup (SGState a) where
  SGState a <> SGState b = SGState (a <> b)

instance (Semigroup a) => RwState (SGState a) where
  type ReadRep (SGState a) = SGState a

writeSGState
  :: (Semigroup a, Monad m)
  => SGState a
  -> SGState a
  -> m (SGState a)
writeSGState e s = return (s <> e)

everyOther :: StateT Int (Rwa (SGState [Int]) Identity) [Int]
everyOther = do
  l <- get
  SGState s <- lift readState
  if length s >= 5
     then return s
     else do if length s > l
                then do put (length s + 1)
                        lift.writeState $ SGState [length s]
                else return ()
             awaitM $ do
               SGState s' <- checkState
               (length s' > length s) ?> everyOther

testRwa = testGroup "Rwa"
  [testCase "StateT" $
     runRwa
       (evalStateT everyOther 0)
       (SGState [])
       writeSGState
       (\(SGState s) -> return.SGState $ s ++ [-1])
     @?= Identity [-1,1,-1,3,-1]
  ]
