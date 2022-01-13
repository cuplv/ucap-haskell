{-# LANGUAGE TypeFamilies #-}

module UCap.Op
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
  -- * Lifters
  , Lifter
  , (>:)
  , (^#)
  , overLf
  , overLf'
  , _1ed
  , _2ed
  , _3ed
  , _4ed
  -- * Operations for 'Data.Num' types
  , module UCap.Op.Num
  -- * Lifters for 'Data.Either.Either'
  , module UCap.Op.Either
  -- * Operations and Lifters for 'Data.Map.Map'
  , module UCap.Op.Map
  ) where

import UCap.Domain
import UCap.Lifter
import UCap.Op.Either
import UCap.Op.Internal
import UCap.Op.Map
import UCap.Op.Num

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
