{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Data.UCap.Either where

import Data.UCap.Classes
import Data.UCap.Const

import Data.Aeson
import GHC.Generics

data EitherE e1 e2
  = OverLR e1 e2
  | SetL (State e1)
  | SetR (State e2)

instance (EffectDom e1, EffectDom e2) => Semigroup (EitherE e1 e2) where
  OverLR e1 e2 <> OverLR f1 f2 = OverLR (e1 <> f1) (e2 <> f2)
  OverLR e1 _ <> SetL s = SetL (eFun e1 s)
  OverLR _ e2 <> SetR s = SetR (eFun e2 s)
  e <> _ = e

instance (EffectDom e1, EffectDom e2) => Monoid (EitherE e1 e2) where
  mempty = OverLR mempty mempty

instance (EffectDom e1, EffectDom e2) => EffectDom (EitherE e1 e2) where
  type State (EitherE e1 e2) = Either (State e1) (State e2)
  eFun (OverLR e1 _) (Left s) = Left (eFun e1 s)
  eFun (OverLR _ e2) (Right s) = Right (eFun e2 s)
  eFun (SetL s) _ = Left s
  eFun (SetR s) _ = Right s

data EitherC c1 c2 s1 s2
  = EitherC { overL :: ConstC c1 s1, overR :: ConstC c2 s2 }
  deriving (Show,Eq,Ord,Generic)

type family EitherC' c1 c2 where
  EitherC' c1 c2 = EitherC c1 c2 (State' c1) (State' c2)

instance
  ( Ord s1
  , Ord s2
  , Semigroup c1
  , Semigroup c2
  ) => Semigroup (EitherC c1 c2 s1 s2) where
  EitherC a b <> EitherC c d = EitherC (a <> c) (b <> d)

instance
  ( Ord s1
  , Ord s2
  , Monoid c1
  , Monoid c2
  ) => Monoid (EitherC c1 c2 s1 s2) where
  mempty = EitherC mempty mempty

instance
  ( Ord s1
  , Ord s2
  , Meet c1
  , Meet c2
  ) => Meet (EitherC c1 c2 s1 s2) where
  meet (EitherC c1 c2) (EitherC d1 d2) =
    EitherC (meet c1 d1) (meet c2 d2)
  EitherC c1 c2 <=? EitherC d1 d2 = c1 <=? d1 && c2 <=? d2

instance
  ( Ord s1
  , Ord s2
  , BMeet c1
  , BMeet c2
  ) => BMeet (EitherC c1 c2 s1 s2) where
  meetId = EitherC meetId meetId

instance
  ( Ord s1
  , Ord s2
  , Split c1
  , Split c2
  ) => Split (EitherC c1 c2 s1 s2) where
  split (EitherC c1 c2) (EitherC d1 d2) =
    EitherC <$> split c1 d1 <*> split c2 d2

instance
  ( Ord s1
  , Ord s2
  , Cap c1
  , Cap c2
  , State' c1 ~ s1
  , State' c2 ~ s2
  ) => Cap (EitherC c1 c2 s1 s2) where
  type Effect (EitherC c1 c2 s1 s2) = EitherE (Effect c1) (Effect c2)
  mincap (OverLR e1 e2) =
    EitherC (mincap (ModifyE e1)) (mincap (ModifyE e2))
  mincap (SetL s1) = EitherC (mincap (ConstE s1)) idC
  mincap (SetR s2) = EitherC idC (mincap (ConstE s2))

  undo (OverLR e1 e2) = EitherC (undo (ModifyE e1)) (undo (ModifyE e2))
  undo _ = idC

  weaken (EitherC c1 c2) (EitherC d1 d2) =
    case (weaken c1 d1, weaken c2 d2) of
      (Just (ModifyE e1), Just (ModifyE e2)) -> Just $ OverLR e1 e2
      _ -> Nothing

onL :: (Monoid c2, Ord (State' c2)) => ConstC' c1 -> EitherC' c1 c2
onL c = EitherC c idC

onR :: (Monoid c1, Ord (State' c1)) => ConstC' c2 -> EitherC' c1 c2
onR c = EitherC idC c
