{-# LANGUAGE FlexibleContexts #-}

module Data.UCap.UCT.Queue where

import Data.UCap.Classes
import Data.UCap.Identity
import Data.UCap.Map
import Data.UCap.UCT

import Data.Map (Map)
import qualified Data.Map as Map

type Queue a = Map Int a

type QueueE a = MapE Int (IdentityE a)

type QueueC a = MapC' Int (IdentityC a)

type QueueT a m = UCT' (QueueC a) m

nextKey :: Map Int a -> Int
nextKey m = if Map.size m == 0
               then 0
               else foldr (\a b -> max a b) 0 (Map.keys m) + 1

smallestElem :: Map Int a -> Maybe (Int,a)
smallestElem m = foldr f Nothing (Map.assocs m)
  where f e1 (Just e2) | fst e1 <= fst e2 = Just e1
        f e1 (Just e2) | fst e1 > fst e2 = Just e2
        f e1 Nothing = Just e1

enqueue :: (Ord a, RVal r m String) => a -> r -> QueueT a m
enqueue a = uctRet
  uniC
  insertAny
  (\m -> let k = nextKey m
         in (mapE k (insertE a), show k))

dequeue :: (Ord a, RVal r m a, Monoid a) => r -> QueueT a m
dequeue = uctRet
  uniC
  deleteAny
  (\m -> case smallestElem m of
           Just (k,a) -> (mapE k deleteE, a)
           Nothing -> (idE, mempty))
