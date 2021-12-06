{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.UCap.Classes
  ( Cap (..)
  , idC
  , uniC
  , cFun
  , EffectDom (..)
  , idE
  , Meet (..)
  , BMeet (..)
  , Split (..)
  , State'
  ) where

{-| An 'EffectDom' is a domain of effects (@e@) on some state type
    (@s@), in which each effect denotes (by 'eFun') a pure
    function on a state value.  The effects must form a
    'Data.Monoid.Monoid' according to the following laws:

@
\-\- Identity
'eFun' 'Data.Monoid.mempty' = 'Data.Function.id'

\-\- Composition
'eFun' (e2 'Data.Semigroup.<>' e1) = 'eFun' e2 'Data.Function..' 'eFun' e1
@

    Note that 'Data.Semigroup.<>' composes effects right-to-left, just
    like function composition.
-}
class (Monoid e) => EffectDom e where
  type State e
  eFun :: e -> State e -> State e

{-| The identity effect, a synonym for 'Data.Monoid.mempty'. -}
idE :: (Monoid e) => e
idE = mempty

instance
  ( EffectDom e1
  , EffectDom e2 )
  => EffectDom (e1,e2) where
  type State (e1,e2) = (State e1, State e2)
  eFun (e1,e2) (s1,s2) =
    ( eFun e1 s1
    , eFun e2 s2 )

-- instance
--   ( EffectDom e1 s1
--   , EffectDom e2 s2
--   , EffectDom e3 s3 )
--   => EffectDom (e1,e2,e3) (s1,s2,s3) where
--   eFun (e1,e2,e3) (s1,s2,s3) =
--     ( eFun e1 s1
--     , eFun e2 s2
--     , eFun e3 s3 )

-- instance
--   ( EffectDom e1 s1
--   , EffectDom e2 s2
--   , EffectDom e3 s3
--   , EffectDom e4 s4 )
--   => EffectDom (e1,e2,e3,e4) (s1,s2,s3,s4) where
--   eFun (e1,e2,e3,e4) (s1,s2,s3,s4) =
--     ( eFun e1 s1
--     , eFun e2 s2
--     , eFun e3 s3
--     , eFun e4 s4 )

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

class (Meet a) => BMeet a where
  meetId :: a

instance BMeet () where
  meetId = ()

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

class (BMeet c, Split c, EffectDom (Effect c)) => Cap c where
  type Effect c
  mincap :: Effect c -> c
  mincap _ = uniC
  
  maxeff :: c -> Maybe (Effect c)
  maxeff c | c == idC = Just idE
           | otherwise = Nothing

  undo :: Effect c -> c
  undo _ = mempty

  weaken :: c -> c -> Maybe (Effect c)
  weaken c1 c2 | meetId <=? c1 = Just idE
               | c2 <=? mempty = Just idE
               | otherwise = Nothing

type family State' c where
  State' c = State (Effect c)

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

cFun :: (Cap c) => c -> Maybe (Effect c)
cFun = maxeff
