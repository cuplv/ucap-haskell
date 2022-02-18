module Lang.Rwa.Interpret
  ( -- * 'RwState'
    -- $RwState
    RwState (..)
    -- * 'RwaTerm'
  , RwaTerm (..)
  , Rwa
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
