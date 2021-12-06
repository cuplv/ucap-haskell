{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Data.UCap.Either where

import Data.UCap.Classes
import Data.UCap.Const

import Data.Aeson
import GHC.Generics

data LRE e1 e2
  = LRE e1 e2
  deriving (Show,Eq,Ord,Generic)

instance (ToJSON e1, ToJSON e2) => ToJSON (LRE e1 e2) where
  toEncoding = genericToEncoding defaultOptions
instance (FromJSON e1, FromJSON e2) => FromJSON (LRE e1 e2)

instance (Semigroup e1, Semigroup e2) => Semigroup (LRE e1 e2) where
  LRE e1 e2 <> LRE f1 f2 = LRE (e1 <> f1) (e2 <> f2)

instance (Monoid e1, Monoid e2) => Monoid (LRE e1 e2) where
  mempty = LRE mempty mempty

instance (EffectDom e1, EffectDom e2) => EffectDom (LRE e1 e2) where
  type State (LRE e1 e2) = Either (State e1) (State e2)
  eFun (LRE e _) (Left s) = Left (eFun e s)
  eFun (LRE _ e) (Right s) = Right (eFun e s)

-- type EitherE e1 e2 = ConstE' (LRE e1 e2)

-- setLeftE :: State e1 -> EitherE e1 e2
-- setLeftE = ConstE . Left

-- setRightE :: State e2 -> EitherE e1 e2
-- setRightE = ConstE . Right

-- onLeftE :: (Monoid e2) => e1 -> EitherE e1 e2
-- onLeftE e1 = ModifyE (LRE e1 idE)

-- onRightE :: (Monoid e1) => e2 -> EitherE e1 e2
-- onRightE e2 = ModifyE (LRE idE e2)

data EitherE e1 e2
  = ModifyLR e1 e2
  | ConstL (State e1)
  | ConstR (State e2)

-- data EitherC c1 c2
--   = EitherC (Maybe c1) (Maybe c2)
--   deriving (Show,Eq,Ord,Generic)

-- instance (ToJSON c1, ToJSON c2) => ToJSON (EitherC c1 c2) where
--   toEncoding = genericToEncoding defaultOptions
-- instance (FromJSON c1, FromJSON c2) => FromJSON (EitherC c1 c2)

-- instance (Semigroup c1, Semigroup c2) => Semigroup (EitherC c1 c2) where
--   EitherC c1 c2 <> EitherC d1 d2 = EitherC (c1 <> d1) (c2 <> d2)

-- type EitherC c1 c2 = ConstC' (c1,c2)

-- setLeftC :: (Monoid c1) => InfSet (State' c1) -> EitherC c1 c2
-- setLeftC s = ConstC
