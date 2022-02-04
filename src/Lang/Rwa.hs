{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Lang.Rwa
  ( -- * 'Rwa'
    Rwa
  , readState
  , writeState
  , await
    -- * 'Block'
  , Block
  , notReady
  , checkState
  , whenBlocked
  , firstOf
  , nonBlock
  , loopBlock
  , onlyWhen
  , (?>)
  , andThen
  , andThen_
    -- * 'Extra'
  , popQ
  , lift
  ) where

import Lang.Rwa.Internal
import Lang.Rwa.Interpret

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Free
import Lens.Micro.Platform

readState :: (Monad m) => Rwa w m (ReadRep w)
readState = wrap $ ReadState return

writeState :: (Monad m) => w -> Rwa w m ()
writeState w = wrap $ WriteState w (return ())

await :: (Monad m) => Block w m (Rwa w m a) -> Rwa w m a
await b = wrap $ Await b

whenBlocked :: (Monad m) => Block w m a -> Block w m a -> Block w m a
whenBlocked b1 b2 = do
  state <- checkState
  r <- lift $ checkBlock b1 state
  case r of
    Just a -> return a
    Nothing -> b2

checkState :: (Monad m) => Block w m (ReadRep w)
checkState = Block ask

{-| A 'Block' term that blocks.

@
printState :: 'Block' ('Maybe' 'String') m ('IO' ())
printState = do
  result <- 'checkState'
  case result of
    'Just' str -> 'return' ('print' str)
    'Nothing' -> 'notReady'
@

In this example, if the state is a 'Just' value, the 'Block' yields a program that prints the value.  If not, it blocks with 'notReady'.
-}
notReady :: (Monad m) => Block w m a
notReady = Block $ throwError ()

onlyWhen :: (Monad m) => Bool -> a -> Block w m a
onlyWhen test cont = if test
                        then return cont
                        else notReady

(?>) :: (Monad m) => Bool -> a -> Block w m a
(?>) = onlyWhen

firstOf :: (Monad m) => [Block w m a] -> Block w m a
firstOf [] = notReady
firstOf (b:bs) = whenBlocked b (firstOf bs)

nonBlock :: (Monad m) => a -> Block w m a
nonBlock = return

loopBlock :: (Monad m) => Block w m (Rwa w m a) -> Rwa w m ()
loopBlock b = await b >> loopBlock b

andThen :: (Monad m, Monad n) => Block w m (n a) -> (a -> n b) -> Block w m (n b)
andThen b f = nonBlock . (f =<<) =<< b

andThen_ :: (Monad m, Monad n) => Block w m (n a) -> n b -> Block w m (n b)
andThen_ b m = andThen b (const m)

popQ :: (MonadState a m) => Lens' a [b] -> Block w m (Rwa w m b)
popQ l = do
  q <- lift $ use l
  not (null q) ?> do 
    lift $ l <<%= drop 1
    return $ head q
