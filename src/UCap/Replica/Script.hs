module UCap.Replica.Script where

import UCap.Domain
import UCap.Op
import UCap.Replica.Capconf

data Script i c a
  = EmitEffect (CEffect c) (Script i c a)
  | ReadCaps (Capconf i c -> Script i c a)
  | ReadState (CState c -> Script i c a)
  | WriteCaps (Capconf i c) (Script i c a)
  | Blocked (Script i c a)
  | Return a

emit :: CEffect c -> Script i c ()
emit e = EmitEffect e (Return ())

readCaps :: Script i c (Capconf i c)
readCaps = ReadCaps Return

readState :: Script i c (CState c)
readState = ReadState Return

writeCaps :: Capconf i c -> Script i c ()
writeCaps cc = WriteCaps cc (Return ())

block :: Script i c ()
block = Blocked (Return ())

instance Functor (Script i c) where
  fmap f sc = case sc of
    EmitEffect e sc' -> EmitEffect e (fmap f sc')
    ReadCaps f2 -> ReadCaps $ fmap f . f2
    ReadState f2 -> ReadState $ fmap f . f2
    WriteCaps cc' sc' -> WriteCaps cc' (fmap f sc')
    Blocked sc' -> Blocked (fmap f sc')
    Return a -> Return (f a)

instance Applicative (Script i c) where
  pure = Return
  sc1 <*> sc2 = case sc1 of
    EmitEffect e1 sc1' -> EmitEffect e1 (sc1' <*> sc2)
    ReadCaps f -> ReadCaps $ \cc -> f cc <*> sc2
    ReadState f -> ReadState $ \s -> f s <*> sc2
    WriteCaps cc1 sc1' -> WriteCaps cc1 (sc1' <*> sc2)
    Blocked sc1' -> Blocked (sc1' <*> sc2)
    Return f -> f <$> sc2

instance Monad (Script i c) where
  return = Return
  sc1 >>= fm = case sc1 of
    EmitEffect e1 sc1' -> EmitEffect e1 (sc1' >>= fm)
    ReadCaps f -> ReadCaps $ \cc -> (f cc) >>= fm
    ReadState f -> ReadState $ \s -> (f s) >>= fm
    WriteCaps cc1 sc1' -> WriteCaps cc1 (sc1' >>= fm)
    Blocked sc1' -> Blocked (sc1' >>= fm)
    Return a -> fm a

data ScriptT i c m a
  = ScriptT { runScriptT :: m (Script i c a) }

-- transact
--   :: (Ord i, Cap c, Monad m)
--   => Op c m () a
--   -> m (Script i c (Maybe a))
-- transact op = undefined
