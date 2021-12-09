{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.UCap.UCT where

import Data.UCap.Classes

import Control.Monad.Identity
import Control.Monad.Writer

data UCT c e s m
  = UCT { uctRead :: c
        , uctWrite :: c
        , uctTrans :: s -> m e
        }

type UCT' c m = UCT c (Effect c) (State' c) m

class (Applicative m) => RVal r m a where
  rval :: r -> a -> m ()

execTrans :: (Cap c, Applicative m) => UCT' c m -> State' c -> m (State' c)
execTrans t s = (\e -> eFun e s) <$> uctTrans t s

execTrans' :: (Cap c, Monad m) => [UCT' c m] -> State' c -> m (State' c)
execTrans' ts s0 = foldM (\s t -> execTrans t s) s0 ts

uct :: (Applicative m) => c -> c -> (State' c -> Effect c) -> UCT' c m
uct r w f = UCT r w (pure . f)

uct' :: c -> c -> (State' c -> m (Effect c)) -> UCT' c m
uct' = UCT

uctW :: (Applicative m, Cap c) => c -> Effect c -> UCT' c m
uctW r e = uct r (mincap e) (const e)

uctRet
  :: (Applicative m, RVal r m a)
  => c
  -> c
  -> (State' c -> (Effect c, a))
  -> r
  -> UCT' c m
uctRet r w f rv = uct' r w f'
  where f' s = case f s of
                 (e,a) -> e <$ rval rv a

data PrintRet = PrintRet

instance (Show a) => RVal PrintRet IO a where
  rval PrintRet a = print a

data WRet = WRet

instance RVal WRet (WriterT [a] Identity) a where
  rval WRet a = tell [a]
