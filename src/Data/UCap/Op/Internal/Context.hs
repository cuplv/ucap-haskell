module Data.UCap.Op.Internal.Context where

import Data.UCap.Classes
import Data.UCap.Op.Internal

import Control.Monad.Except

runBody :: OpBody c m a -> CState c -> m (CEffect c, a)
runBody (OpBody b) = b

data OpContext c m
  = OpContext { getState :: m (CState c)
              , emitEffect :: CEffect c -> m ()
              , remoteCap :: m c
              }

runOp
  :: (Monad m, Cap c)
  => OpContext c m
  -> c
  -> Op c () m a
  -> ExceptT () m (c, a)
runOp ctx lc (Op r w p b) = do
  rc <- lift $ remoteCap ctx
  if rc <=? r && w <=? lc
     then return ()
     else throwError ()
  (e,a) <- lift $ getState ctx >>= runBody (b ())
  lc' <- case split lc (mincap e) of
           Just c -> return $ c <> undo e
           Nothing -> throwError ()
  lift $ emitEffect ctx e
  return (lc',a)
