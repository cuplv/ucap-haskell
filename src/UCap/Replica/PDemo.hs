{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module UCap.Replica.PDemo where

import UCap.Domain
import UCap.Lens
import UCap.Replica
import UCap.Replica.Demo

import Control.Monad.State
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as Map

type PScript i c m = ScriptT i c m ()

type BlockStatus i c = Maybe (CState c, Capconf i c)

type PState i c m = Map i (BlockStatus i c, PScript i c m)

type PDemo i c m = StateT (PState i c m) (DemoState i c m)

runPDemo
  :: (Ord i, Cap c, Monad m)
  => CState c
  -> RState i c
  -> PState i c m
  -> PDemo i c m a
  -> m ((a, PState i c m), RState i c)
runPDemo s0 rs0 p0 act =
  runDemo s0 rs0 (runStateT act p0)

evalPDemo
  :: (Ord i, Cap c, Monad m)
  => Map i (PScript i c m)
  -> Capconf i c
  -> CState c
  -> PDemo i c m a
  -> m a
evalPDemo psc0 cc0 s0 act =
  fst
  <$> evalDemo
        (Map.keys psc0)
        cc0
        s0
        (runStateT act (Map.map ((,) Nothing) psc0))

replicaIds :: (Ord i, Monad m) => PDemo i c m [i]
replicaIds = Map.keys <$> get

repScript
  :: (Ord i)
  => i
  -> Lens' (PState i c m) (Maybe (BlockStatus i c, PScript i c m))
repScript i = at i

getScript
  :: (Ord i, Cap c, Monad m)
  => i
  -> PDemo i c m (Maybe (PScript i c m))
getScript i = do
  scm <- use $ repScript i
  case scm of
    Just (Nothing,sc) -> return (Just sc)
    Just (Just _,_) -> return Nothing
    Nothing -> error "Replica had no script."

updateBlock :: (Eq (CState c), Ord i, Cap c, Monad m) => i -> PDemo i c m ()
updateBlock i = do
  scm <- use $ repScript i
  case scm of
    Just (Just (s0,cc0), sc) -> do
      s <- lift $ stateD i
      cc <- lift $ use (capsL i)
      if s == s0 && cc == cc0
         then return ()
         else repScript i .= Just (Nothing, sc)
    _ -> return ()

-- regUpdate :: (Ord i, Cap c, Monad m) => i -> PDemo i c m ()
-- regUpdate i = do
--   ps <- filter (/= i) . Map.keys <$> use _2
--   traverse_ clearBlock ps

stepPD :: (Ord i, Cap c, Monad m) => i -> PDemo i c m Bool
stepPD i = do
  scm <- getScript i
  undefined
