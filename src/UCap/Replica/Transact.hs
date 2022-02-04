module UCap.Replica.Transact
  ( transactSimple
  , transact
  , transactMany
  , transactMany_
  , acquireLock
  , grantLock
  ) where

import UCap.Domain
import UCap.Lens
import UCap.Op
import UCap.Replica.Capconf
import UCap.Replica.Coord
import UCap.Replica.Script

import Data.Maybe (fromJust)

{-| Compile an operation into a replica script.  If capabilities are not
  sufficient, 'Nothing' is returned. -}
transactSimple
  :: (Ord i, Cap c, Monad m)
  => Op c m () a
  -> ScriptT i c m (Maybe a)
transactSimple op = do
  rid <- getReplicaId
  ctx <- readState
  let s = ctx ^. rsStore
  let cc = ctx ^. rsCapconf
  let caps = Caps (remoteG' rid cc) (localG rid cc)
  case execWith caps s op of
    Just act -> do
      (_,e,b) <- liftScript act
      case consumeG rid e cc of
        Just cc' -> do
          emitEffect e
          setCapconf cc'
          return (Just b)
    Nothing -> return Nothing

{-| Compile an operation into a replica script.  Capabilities are
  requested if necessary, and the returned await-clause waits for
  them. -}
transact
  :: (Ord i, Cap c, Monad m)
  => Op c m () a
  -> ScriptT i c m (ScriptB i c m a)
transact op = do
  r <- transactSimple op
  case r of
    Just a -> return . nonBlock . return $ a
    Nothing -> do
      b <- acquireLock
      return $ b `andThen_` do
        r <- transactSimple op
        case r of
          Just a -> return a
          Nothing -> error "Transaction not enabled even with lock."

{-| Acquire lock, or pass immediately if lock is already owned. -}
acquireLock :: (Ord i, Cap c, Monad m) => ScriptT i c m (ScriptB i c m ())
acquireLock = do
  rid <- getReplicaId
  onCoord $ requestLock rid
  return $ do
    cd <- view rsCoord <$> checkState
    ownsLock rid cd ?> onCapconf (acceptG rid)

{-| Block until this replica is the lock owner and the lock is requested
  by another, and then grant it. -}
grantLock :: (Ord i, Cap c, Monad m) => ScriptB i c m ()
grantLock = do
  let lockOwnerCap = uniC
  let nonOwnerCap = idC
  ctx <- checkState
  rid <- lift getReplicaId
  isRequestedOf rid (ctx ^. rsCoord) ?> do
    newOwner <- onCoord' $ grantReq rid
    onCapconf $ fromJust . transferG rid (newOwner,lockOwnerCap)
    onCapconf $ mdropG rid nonOwnerCap

{-| Run a list of transactions in sequence, serving coordination requests
  in between and when blocked. -}
transactMany
  :: (Ord i, Cap c, Monad m)
  => [Op c m () a]
  -> ScriptT i c m [a]
transactMany [] = return []
transactMany (o1:os) = do
  complete1 <- transact o1
  await . firstOf $
    [ grantLock `andThen_` transactMany (o1:os)
    , complete1 `andThen` (\a -> (a :) <$> transactMany os)
    ]

{-| A version of 'transactMany' which discard return values.  This is
  useful if the operation list is infinite, and you don't want to
  generate an infinite output list that won't be used. -}
transactMany_
  :: (Ord i, Cap c, Monad m)
  => [Op c m () ()]
  -> ScriptT i c m ()
transactMany_ [] = return ()
transactMany_ (o1:os) = do
  complete1 <- transact o1
  await . firstOf $
    [ grantLock `andThen_` transactMany_ (o1:os)
    , complete1 `andThen_` transactMany_ os
    ]
