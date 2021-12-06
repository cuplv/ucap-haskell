{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.UCap.Const where

import Data.UCap.Classes
import Data.UCap.InfSet (InfSet)
import qualified Data.UCap.InfSet as IS

import Data.Aeson
import GHC.Generics

{-| t'ConstE' wraps an effect domain (@e@), adding an effect that
    replaces the current state with a given value.

@
'eFun' ('ConstE' s) = 'Data.Function.const' s = (\\_ -> s)
@

    Effects from the wrapped domain can be used with 'ModifyE'.

@
'eFun' ('ModifyE' e) s = 'eFun' e s
@
-} 
data ConstE e s
  = ConstE s
  | ModifyE e
  deriving (Show,Eq,Ord,Generic)

instance (ToJSON e, ToJSON s) => ToJSON (ConstE e s) where
  toEncoding = genericToEncoding defaultOptions
instance (FromJSON e, FromJSON s) => FromJSON (ConstE e s)

instance (EffectDom e, State e ~ s) => Semigroup (ConstE e s) where
  ConstE s <> _ = ConstE s
  ModifyE e2 <> ModifyE e1 = ModifyE (e2 <> e1)
  ModifyE e <> ConstE s = ConstE (eFun e s)

instance (EffectDom e, State e ~ s) => Monoid (ConstE e s) where
  mempty = ModifyE mempty

instance (EffectDom e, State e ~ s) => EffectDom (ConstE e s) where
  type State (ConstE e s) = s
  eFun (ConstE s) = const s
  eFun (ModifyE e) = eFun e

type family ConstE' e where
  ConstE' e = ConstE e (State e)

data ConstC c s
  = ConstC { setVals :: InfSet s, lowerC :: c }
  deriving (Eq,Ord,Generic)

type family ConstC' c where
  ConstC' c = ConstC c (State' c)

instance (Ord s, Show c, Show s, BMeet c, Monoid c) => Show (ConstC c s) where
  show c | uniC <=? c = "uniC"
  show c | c <=? idC = "idC"
  show (ConstC s c) | IS.isEmpty s = show c

instance (ToJSON c, ToJSON s) => ToJSON (ConstC c s)
instance (Ord s, ToJSON c, ToJSONKey c, ToJSON s, ToJSONKey s) => ToJSONKey (ConstC c s)
instance (Ord s, FromJSON c, FromJSON s) => FromJSON (ConstC c s)
instance (Ord s, FromJSON c, FromJSONKey c, FromJSON s, ToJSONKey s) => FromJSONKey (ConstC c s)

instance (Ord s, Semigroup c) => Semigroup (ConstC c s) where
  ConstC s1 c1 <> ConstC s2 c2 =
    ConstC (IS.union s1 s2) (c1 <> c2)

instance (Ord s, Monoid c) => Monoid (ConstC c s) where
  mempty = ConstC IS.empty mempty

instance (Ord s, Meet c) => Meet (ConstC c s) where
  meet (ConstC s1 c1) (ConstC s2 c2) =
    ConstC (s1 `IS.intersection` s2) (c1 `meet` c2)

  ConstC s1 c1 <=? ConstC s2 c2 = (s1 `IS.isSubsetOf` s2) && (c1 <=? c2)

instance (Ord s, BMeet c) => BMeet (ConstC c s) where
  meetId = ConstC IS.universal meetId

instance (Ord s, Split c) => Split (ConstC c s) where
  split (ConstC s1 c1) (ConstC s2 c2) =
    if s2 `IS.isSubsetOf` s1
       then ConstC s1 <$> split c1 c2
       else Nothing

instance (Ord s, Meet c, Cap c, Split c, State' c ~ s) => Cap (ConstC c s) where
  type Effect (ConstC c s) = ConstE (Effect c) s
  mincap (ConstE s) = ConstC (IS.singleton s) mempty
  mincap (ModifyE e) = ConstC IS.empty (mincap e)

  undo (ConstE _) = idC
  undo (ModifyE e) = modifyC (undo e)

  weaken (ConstC s1 c1) (ConstC s2 c2)
    | s2 <=? s1 = ModifyE <$> weaken c1 c2
  weaken _ _ = Nothing

constC :: (Ord s, Monoid c) => [s] -> ConstC c s
constC ss = ConstC (IS.fromList ss) mempty

constC' :: (Monoid c) => InfSet s -> ConstC c s
constC' s = ConstC s mempty

modifyC :: c -> ConstC c s
modifyC c = ConstC IS.empty c
