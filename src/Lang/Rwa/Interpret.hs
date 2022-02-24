{-# LANGUAGE LambdaCase #-}

module Lang.Rwa.Interpret
  ( -- * 'RwState'
    -- $RwState
    RwState (..)
    -- * 'RwaTerm'
  , RwaTerm (..)
  , Rwa
  , MonadRwa (..)
  , runRwa
  , nextTerm
    -- * 'Block'
  , Block (..)
  , checkBlock
  , wrap
  ) where

import Lang.Rwa.Internal

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Free

{-| Run an action in the underlying monad to unwrap either the program's
  return value (in 'Right') or the next 'RwaTerm' (in 'Left') if it is
  not complete. -}
nextTerm
  :: (Monad m)
  => Rwa w m a
  -> m (Either (RwaTerm w m (Rwa w m a)) a)
nextTerm p = runFreeT p >>= \m ->
  case m of
    Free t -> return (Left t)
    Pure a -> return (Right a)

{-| Run an action in the underlying monad to check, given a snapshot of
  the state, whether the block is clear.  If so, the continuation is
  returned in 'Just'. -}
checkBlock
  :: (Monad m)
  => Block w m a
  -> ReadRep w
  -> m (Maybe a)
checkBlock (Block m) state = do
  r <- runReaderT (runExceptT m) state
  case r of
    Right a -> return $ Just a
    Left () -> return $ Nothing

{-| Run an 'Rwa' script, using an initial state, a way of updating the
  state using an 'RwState' value, and a background state-updating
  action to take when the script blocks.  The state-updating action
  continues to run until the block is cleared. -}
runRwa
  :: (Monad m)
  => Rwa w m a
  -> ReadRep w -- ^ State value
  -> (w -> ReadRep w -> m (ReadRep w)) -- ^ Write action
  -> (ReadRep w -> m (ReadRep w)) -- ^ Background update action
  -> m a
runRwa sc s write bg = nextTerm sc >>= \case
  Left (ReadState f) -> runRwa (f s) s write bg
  Left (WriteState w sc) -> do
    s <- write w s
    runRwa sc s write bg
  Left (Await b) -> do
    (s,sc) <- runBlock b bg s
    runRwa sc s write bg
  Right a -> return a

runBlock
  :: (Monad m)
  => Block w m a
  -> (ReadRep w -> m (ReadRep w))
  -> ReadRep w
  -> m (ReadRep w, a)
runBlock b update s = checkBlock b s >>= \case
  Just a -> return (s,a)
  Nothing -> update s >>= runBlock b update

{- $RwState
The 'RwState' class is for pairs of types that represent different representations for reading and writing on some state.

The following example gives types to work with an append-only list state of type @[a]@.

@
newtype AddElem a = AddElem a

instance 'RwState' (AddElem a) where
  type 'ReadRep' (AddElem a) = [a]
@

Here, @'AddElem' a@ is the write representation, and @[a]@ as the read representation.
-}
