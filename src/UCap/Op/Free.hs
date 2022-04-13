{-# LANGUAGE TypeFamilies #-}

module UCap.Op.Free where

import UCap.Domain.Classes
import UCap.Domain.Free
import UCap.Op.Internal

import Control.Arrow

-- | Set the state to the given value.  Pass any input through to output.
freeSet :: (Applicative m, Cap c, CEffect c ~ FreeE s) => s -> Op c m a a
freeSet = effect . FreeSet

-- | Apply a function to the state.  Return the pre and post states.
freeMod :: (Monad m, Cap c, CEffect c ~ FreeE s) => (s -> s) -> Op c m a (s,s)
freeMod f =
  query idC 
  >>> mapOp f
  >>> Op uniC uniC idC (\_ -> OpBody $ \s -> let s' = f s
                                             in pure (FreeSet s', (s,s')))

-- | Apply a monadic function to the state.  Return the pre and post states.
freeModM :: (Monad m, Cap c, CEffect c ~ FreeE s) => (s -> m s) -> Op c m a (s,s)
freeModM f =
  query idC 
  >>> mapOp f
  >>> Op uniC uniC idC (\_ -> OpBody $ \s -> let s' = f s
                                             in (\s' -> (FreeSet s', (s,s')))
                                                <$> s')
