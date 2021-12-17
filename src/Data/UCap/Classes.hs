{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.UCap.Classes
  ( Cap (..)
  , idC
  , uniC
  , EffectDom (..)
  , idE
  , Meet (..)
  , BMeet (..)
  , Split (..)
  , CState
  ) where

{-| An 'EffectDom' is a domain of effects (@e@) on some state type
    (@'EDState' e@), in which each effect denotes (by 'eFun') a pure
    function on a state value.  The effects must form a
    'Data.Monoid.Monoid' according to the following laws:

@
\-\- Identity
'eFun' 'Data.Monoid.mempty' = 'Data.Function.id'

\-\- Composition
'eFun' (e1 'Data.Semigroup.<>' e2) = 'eFun' e2 'Data.Function..' 'eFun' e1
@

    Note that 'Data.Semigroup.<>' composes effects left-to-right,
    unlike the right-to-left 'Data.Function..' composition it relates
    to.
-}
class (Monoid e) => EffectDom e where
  type EDState e
  eFun :: e -> EDState e -> EDState e

{-| The identity effect, a synonym for 'Data.Monoid.mempty'. -}
idE :: (Monoid e) => e
idE = mempty

instance
  ( EffectDom e1
  , EffectDom e2 )
  => EffectDom (e1,e2) where
  type EDState (e1,e2) = (EDState e1, EDState e2)
  eFun (e1,e2) (s1,s2) =
    ( eFun e1 s1
    , eFun e2 s2 )

class Meet a where
  meet :: a -> a -> a
  (<=?) :: a -> a -> Bool
  a <=? b | compareP a b == Just LT = True
          | otherwise = False

  compareP :: a -> a -> Maybe Ordering
  compareP a b | a <=? b && b <=? a = Just EQ
               | a <=? b = Just LT
               | b <=? a = Just GT
               | otherwise = Nothing

instance Meet () where
  meet () () = ()
  () <=? () = True

instance (Meet a, Meet b) => Meet (a,b) where
  meet (a1,b1) (a2,b2) = (meet a1 a2, meet b1 b2)
  (a1,b1) <=? (a2,b2) = (a1 <=? a2) && (b1 <=? b2)

class (Meet a) => BMeet a where
  meetId :: a

instance BMeet () where
  meetId = ()

instance (BMeet a, BMeet b) => BMeet (a,b) where
  meetId = (meetId, meetId)

{-| 'Split' is related to the 'Monoid' implementation by the following
    law:

@
'split' a1 a2 = 'Maybe.Just' a3
iff
a1 = a2 'Semigroup.<>' a3
@
-}
class (Eq a, Monoid a) => Split a where
  split :: a -> a -> Maybe a
  split a1 a2 | a1 == a2 = Just mempty
              | a2 == mempty = Just a1
              | otherwise = Nothing

instance Split ()

instance (Split a, Split b) => Split (a,b) where
  split (a1,b1) (a2,b2) = (,) <$> split a1 a2 <*> split b1 b2

class (BMeet c, Split c, EffectDom (CEffect c)) => Cap c where
  type CEffect c
  mincap :: CEffect c -> c
  mincap _ = uniC

  undo :: CEffect c -> c
  undo _ = mempty

  weaken :: c -> c -> Maybe (CEffect c)
  weaken c1 c2 | meetId <=? c1 = Just idE
               | c2 <=? mempty = Just idE
               | otherwise = Nothing

instance (Cap a, Cap b) => Cap (a,b) where
  type CEffect (a,b) = (CEffect a, CEffect b)
  mincap (a,b) = (mincap a, mincap b)
  undo (a,b) = (undo a, undo b)
  weaken (a1,b1) (a2,b2) = (,) <$> weaken a1 a2 <*> weaken b1 b2

type family CState c where
  CState c = EDState (CEffect c)

{-| The identity capability, which permits only the identity effect.
  This is a synonym for 'mempty'.
-}
idC :: (Monoid c) => c
idC = mempty

{-| The universal capability, permitting every effect.  This is a
  synonym for 'meetId'.
-}
uniC :: (BMeet c) => c
uniC = meetId
