{-# LANGUAGE TypeFamilies #-}

module Data.UCap.Op where

import Data.UCap.Classes

data Op c e s a
  = Op { opRead :: c
       , opWrite :: c
       , opBody :: s -> (e,a)
       }

type Op' c = Op c (Effect c) (State' c)

instance Functor (Op c e s) where
  fmap f (Op r w b) = Op r w $ (fmap.fmap) f b

instance
  ( Monoid c
  , BMeet c
  , EffectDom e
  , State e ~ s
  ) => Applicative (Op c e s) where
  pure a = Op uniC idC (const $ (idE,a))
  (Op r1 w1 b1) <*> (Op r2 w2 b2) = 
    Op (r1 `meet` r2) (w1 <> w2) $ \s ->
      let (e1,f) = b1 s
          (e2,a) = b2 (eFun e1 s)
      in (e2 <> e1, f a)
