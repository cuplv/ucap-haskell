module UCap.Replica.Script
  ( ScriptF (..)
  , ScriptT
  , unwrapScript
  , getReplicaId
  , readCaps
  , readState
  , writeCaps
  , writeState
  , block
  , ACond
  , AwaitBs
  , transact
  ) where

import UCap.Domain
import UCap.Op
import UCap.Replica.Capconf

import Control.Monad.Reader
import Control.Monad.Trans.Free

type ACond i c m = (Capconf i c, CState c) -> m Bool

type AwaitBs i c m a = [(ACond i c m, a)]

data ScriptF i c m a
  = ReadCaps (Capconf i c -> a)
  | ReadState (CState c -> a)
  | WriteCaps (Capconf i c) a
  | WriteState (CEffect c) a
  | Await (AwaitBs i c m a)

instance Functor (ScriptF i c m) where
  fmap f sc = case sc of
                ReadCaps f1 -> ReadCaps (f . f1)
                ReadState f1 -> ReadState (f . f1)
                WriteCaps cc a -> WriteCaps cc (f a)
                WriteState e a -> WriteState e (f a)
                Await as -> Await $ map (\(ac,a) -> (ac, f a)) as

type ScriptT i c m a = FreeT (ScriptF i c m) (ReaderT i m) a

getReplicaId :: (Monad m) => ScriptT i c m i
getReplicaId = ask

readCaps :: (Monad m) => ScriptT i c m (Capconf i c)
readCaps = wrap $ ReadCaps return

readState :: (Monad m) => ScriptT i c m (CState c)
readState = wrap $ ReadState return

writeCaps :: (Monad m) => Capconf i c -> ScriptT i c m ()
writeCaps cc = wrap $ WriteCaps cc (return ())

writeState :: (Monad m) => CEffect c -> ScriptT i c m ()
writeState e = wrap $ WriteState e (return ())

await :: (Monad m) => AwaitBs i c m (ScriptT i c m a) -> ScriptT i c m a
await acs = wrap $ Await acs

block :: (Monad m) => ACond i c m -> ScriptT i c m ()
block ac = await [(ac, return ())]

{-| Compile an operation into a replica script. -}
transact
  :: (Ord i, Cap c, Monad m)
  => Op c m () a
  -> ScriptT i c m (Maybe a)
transact op = do
  rid <- getReplicaId
  cc <- readCaps
  let caps = Caps (remoteG' rid cc) (localG rid cc)
  s <- readState
  case execWith caps s op of
    Just act -> do
      (_,e,b) <- liftScript act
      case consumeG rid e cc of
        Just cc' -> do
          writeState e
          writeCaps cc'
          return (Just b)
    Nothing -> return Nothing

{-| Perform an action on the underlying monad of the script. -}
liftScript :: (Monad m) => m a -> ScriptT i c m a
liftScript = lift . lift

{-| Get the first term of a script, wrapped in 'Left', or the return
  value of the script, wrapped in 'Right'. -}
unwrapScript
  :: (Monad m)
  => ScriptT i c m a
  -> i
  -> m (Either (ScriptF i c m (ScriptT i c m a)) a)
unwrapScript sc i = runReaderT (runFreeT sc) i >>= \t ->
  case t of
    Free s -> return $ Left s
    Pure a -> return $ Right a
