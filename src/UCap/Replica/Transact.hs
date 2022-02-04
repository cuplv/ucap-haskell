module UCap.Replica.Transact
  ( transactSimple
  , transact
  ) where

import UCap.Domain
import UCap.Lens
import UCap.Op
import UCap.Replica.Capconf
import UCap.Replica.Coord
import UCap.Replica.Script

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
  -- This must start with a ScriptT part because it might need to send
  -- the request for capabilities.
  capsAreSufficient <- liftScript (undefined :: m Bool) -- check the caps
  if capsAreSufficient
     then do b <- undefined op -- exec the op
             return $ nonStop b
     else do undefined -- request the caps
             after
               undefined -- check the caps again
               (do b <- undefined op -- exec the op again
                   return b)

-- transact op = do
--   rid <- getReplicaId
--   ctx <- readState
--   let s = ctx ^. rsStore
--   let cc = ctx ^. rsCapconf
--   let cd = ctx ^. rsCoord
--   let caps = Caps (remoteG' rid cc) (localG rid cc)
--   case execWith caps s op of
--     Just act -> do
--       (_,e,b) <- liftScript act
--       case consumeG rid e cc of
--         Just cc' -> do
--           emitEffect e
--           setCapconf cc'
--           return $ nonStop b
--     Nothing -> do
--       setCoord (requestLock rid cd)
--       undefined
