module UCap.Op.Int where

import UCap.Domain.Classes
import UCap.Domain.Int
import UCap.Op.Internal

import Control.Monad.Except

upperBound :: (Applicative m) => Op IntC m a Int
upperBound = query subAny

atMost :: (Monad m) => Op IntC m Int Bool
atMost = pairOp idOp upperBound `pipe` mapOp (\(x,s) -> x >= s)

lowerBound :: (Applicative m) => Op IntC m a Int
lowerBound = query addAny

atLeast :: (Monad m) => Op IntC m Int Bool
atLeast = pairOp idOp lowerBound `pipe` mapOp (\(x,s) -> x <= s)

addOp :: (Applicative m) => Int -> Op IntC m a Int
addOp n = effect' (addE n) n

addOp' :: (Applicative m) => Op IntC m Int Int
addOp' = Op uniC addAny idC $ \n -> OpBody $ \_ -> pure (addE n, n)

subOp :: (Applicative m) => Int -> Op IntC m a Int
subOp n = effect' (subE n) n

subOp' :: (Applicative m) => Op IntC m Int Int
subOp' = Op uniC subAny idC $ \n -> OpBody $ \_ -> pure (subE n, n)

subGuard :: (Monad m) => Int -> Int -> Op IntC (ExceptT () m) a Int
subGuard lim amt = assert (withInput lim atLeast) *> subOp amt

addGuard :: (Monad m) => Int -> Int -> Op IntC (ExceptT () m) a Int
addGuard lim amt = assert (withInput lim atMost) *> addOp amt
