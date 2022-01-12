{-# LANGUAGE TypeFamilies #-}

module Data.UCap.Op
  (
  -- * Operations
    Op
  , evalOp
  , execOp
  , execWith
  , (*>=)
  , idOp
  , query
  , effect
  , withInput
  , mapOp
  , mapOp'
  , pairOp
  , assert
  -- * Editors
  , Editor
  , (>:)
  , (^#)
  , overEd
  , overEd'
  , _1ed
  , _2ed
  , _3ed
  , _4ed
  -- * Operations for Counter
  , module Data.UCap.Op.Counter
  -- * Editors for Either
  , module Data.UCap.Op.Either
  -- * Operations and Editors for Map
  , module Data.UCap.Op.Map
  ) where

import Data.UCap
import Data.UCap.Editor
import Data.UCap.Op.Counter
import Data.UCap.Op.Either
import Data.UCap.Op.Internal
import Data.UCap.Op.Map

-- | Evaluate a simple operation, which does not interact with a
-- replicated state, to its return value.
evalOp :: (Functor m) => Op (IdentityC ()) () m b -> m b
evalOp o = case execWith fullCaps () o of
  Just m -> (\(_,_,b) -> b) <$> m

-- | Execute an operation on a state value, returning the updated
-- state and the return value.
execOp
  :: (Functor m, Cap c)
  => CState c
  -> Op c () m b
  -> m (CState c, b)
execOp s o = case execWith fullCaps s o of
  Just m -> (\(_,e,b) -> (eFun e s, b)) <$> m

-- | Execute an operation under particular available capabilities,
-- returning the updated capabilities, the produced effect, and the
-- return value.
--
-- If the provided capabilities are not sufficient,
-- 'Data.Maybe.Nothing' is returned.
execWith
  :: (Functor m, Cap c)
  => Caps c
  -> CState c
  -> Op c () m b
  -> Maybe (m (Caps c, CEffect c, b))
execWith (Caps cr cw) s (Op r w p b) = case b () of
  OpBody f -> case split cw w of
    Just w' | cr <=? r -> Just $ fmap
      (\(e,b) -> if not (p <=? undo e) || not (mincap e <=? w)
                    then error $ "Write failure."
                    else let caps' = Caps cr (w' <> undo e)
                         in (caps',e,b))
                 
      (f s)
    Nothing -> Nothing
