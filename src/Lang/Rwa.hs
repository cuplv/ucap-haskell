{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Lang.Rwa where

import Control.Monad.State
import Control.Monad.Trans.Free
import Lens.Micro.Platform

class RwState w where
  type ReadRep w

type AwaitBF w m a = (ReadRep w -> m Bool, a)

type AwaitB w m a = AwaitBF w m (Rwa w m a)

data RwaF w m a
  = ReadState (ReadRep w -> a)
  | WriteState w a
  | Await [AwaitBF w m a]

instance Functor (RwaF w m) where
  fmap f sc = case sc of
                ReadState f1 -> ReadState (f . f1)
                WriteState w a  -> WriteState w (f a)
                Await acs -> Await $ map (\(ac,a) -> (ac, f a)) acs

type Rwa w m a = FreeT (RwaF w m) m a

type RwaTerm w m a = RwaF w m (Rwa w m a)

nextTerm :: (Monad m) => Rwa w m a -> m (Either (RwaTerm w m a) a)
nextTerm p = runFreeT p >>= \m ->
  case m of
    Free t -> return (Left t)
    Pure a -> return (Right a)

readState :: (Monad m) => Rwa w m (ReadRep w)
readState = wrap $ ReadState return

writeState :: (Monad m) => w -> Rwa w m ()
writeState w = wrap $ WriteState w (return ())

await :: (Monad m) => [AwaitB w m a] -> Rwa w m a
await acs = wrap $ Await acs

block :: (Monad m) => (ReadRep w -> Bool) -> Rwa w m ()
block pr = await [(return . pr, return ())]

popQ :: (MonadState a m) => Lens' a [b] -> AwaitB w m b
popQ l =
  let test = not . null <$> use l
      cont = head <$> lift (l <<%= drop 1)
  in (const test, cont)
