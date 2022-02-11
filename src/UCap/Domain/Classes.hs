{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module UCap.Domain.Classes
  ( Cap (..)
  , idC
  , uniC
  , EffectDom (..)
  , idE
  , Meet (..)
  , BMeet (..)
  , Split (..)
  , splitWF
  , WhenFail (..)
  , failMempty
  , failToEither
  , (<<$$>>)
  , (<<*>>)
  , CState
  , Caps (..)
  , emptyCaps
  , fullCaps
  , isId
  , isUni
  , leqCaps
  ) where

import Data.Bifunctor
import Data.Biapplicative

comm3 a b c = (a,b,c)

comm4 a b c d = (a,b,c,d)

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

instance
  ( EffectDom e1
  , EffectDom e2
  , EffectDom e3 )
  => EffectDom (e1,e2,e3) where
  type EDState (e1,e2,e3) = (EDState e1, EDState e2, EDState e3)
  eFun (e1,e2,e3) (s1,s2,s3) =
    ( eFun e1 s1
    , eFun e2 s2
    , eFun e3 s3 )

instance
  ( EffectDom e1
  , EffectDom e2
  , EffectDom e3
  , EffectDom e4 )
  => EffectDom (e1,e2,e3,e4) where
  type EDState (e1,e2,e3,e4) = (EDState e1, EDState e2, EDState e3, EDState e4)
  eFun (e1,e2,e3,e4) (s1,s2,s3,s4) =
    ( eFun e1 s1
    , eFun e2 s2
    , eFun e3 s3
    , eFun e4 s4 )

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

instance (Meet a, Meet b, Meet c) => Meet (a,b,c) where
  meet (a1,b1,c1) (a2,b2,c2) = (meet a1 a2, meet b1 b2, meet c1 c2)
  (a1,b1,c1) <=? (a2,b2,c2) = (a1 <=? a2) && (b1 <=? b2) && (c1 <=? c2)

instance (Meet a, Meet b, Meet c, Meet d) => Meet (a,b,c,d) where
  meet (a1,b1,c1,d1) (a2,b2,c2,d2) = (meet a1 a2, meet b1 b2, meet c1 c2, meet d1 d2)
  (a1,b1,c1,d1) <=? (a2,b2,c2,d2) = (a1 <=? a2) && (b1 <=? b2) && (c1 <=? c2) && (d1 <=? d2)

class (Meet a) => BMeet a where
  meetId :: a

instance BMeet () where
  meetId = ()

instance (BMeet a, BMeet b) => BMeet (a,b) where
  meetId = (meetId, meetId)

instance (BMeet a, BMeet b, BMeet c) => BMeet (a,b,c) where
  meetId = (meetId, meetId, meetId)

instance (BMeet a, BMeet b, BMeet c, BMeet d) => BMeet (a,b,c,d) where
  meetId = (meetId, meetId, meetId, meetId)

{-| 'Split' is related to the 'Monoid' implementation by the following
    law:

@
'split' a1 a2 = 'Data.Either.Right' a3
iff
a1 = a2 'Semigroup.<>' a3
@
-}
class (Meet a, Monoid a) => Split a where
  split :: a -> a -> Either a a
  split a1 a2 | a2 <=? a1 = Right mempty
              | otherwise = Left a2

splitWF :: (Split a) => a -> a -> WhenFail a a
splitWF a1 a2 = case split a1 a2 of
                 Right a3 -> failMempty a3
                 Left a3 -> DidFail a3

instance Split ()

instance (Split a, Split b) => Split (a,b) where
  split (a1,b1) (a2,b2) = failToEither $
    (,) <<$$>> splitWF a1 a2 <<*>> splitWF b1 b2

instance (Split a, Split b, Split c) => Split (a,b,c) where
  split (a1,b1,c1) (a2,b2,c2) = failToEither $
    (,,) <<$$>> splitWF a1 a2 <<*>> splitWF b1 b2 <<*>> splitWF c1 c2

instance (Split a, Split b, Split c, Split d) => Split (a,b,c,d) where
  split (a1,b1,c1,d1) (a2,b2,c2,d2) = failToEither $
    (,,,) <<$$>> splitWF a1 a2 <<*>> splitWF b1 b2 <<*>> splitWF c1 c2 <<*>> splitWF d1 d2

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

instance (Cap a, Cap b, Cap c) => Cap (a,b,c) where
  type CEffect (a,b,c) = (CEffect a, CEffect b, CEffect c)
  mincap (a,b,c) = (mincap a, mincap b, mincap c)
  undo (a,b,c) = (undo a, undo b, undo c)
  weaken (a1,b1,c1) (a2,b2,c2) = comm3 <$> weaken a1 a2 <*> weaken b1 b2 <*> weaken c1 c2

instance (Cap a, Cap b, Cap c, Cap d) => Cap (a,b,c,d) where
  type CEffect (a,b,c,d) = (CEffect a, CEffect b, CEffect c, CEffect d)
  mincap (a,b,c,d) = (mincap a, mincap b, mincap c, mincap d)
  undo (a,b,c,d) = (undo a, undo b, undo c, undo d)
  weaken (a1,b1,c1,d1) (a2,b2,c2,d2) = comm4 <$> weaken a1 a2 <*> weaken b1 b2 <*> weaken c1 c2 <*> weaken d1 d2

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

{-| A monad representing failure, like 'Either' is used. The difference
  here is that even when successful, an error value is recorded to be
  used if failure occurs further down the computation. -}

data WhenFail e a
  = WhenFail e a
  | DidFail e
  deriving (Show,Eq,Ord)

instance Functor (WhenFail e) where
  fmap f (WhenFail e a) = WhenFail e (f a)
  fmap _ (DidFail e) = DidFail e

instance Bifunctor WhenFail where
  bimap fe fa (WhenFail e a) = WhenFail (fe e) (fa a)
  bimap fe _ (DidFail e) = DidFail (fe e)

instance Biapplicative WhenFail where
  bipure = WhenFail

  WhenFail fe fa <<*>> WhenFail e a = WhenFail (fe e) (fa a)
  WhenFail fe fa <<*>> DidFail e = DidFail (fe e)
  DidFail fe <<*>> WhenFail e a = DidFail (fe e)
  DidFail fe <<*>> DidFail e = DidFail (fe e)

failMempty :: (Monoid a) => a -> WhenFail a a
failMempty a = WhenFail mempty a

failToEither :: WhenFail e a -> Either e a
failToEither (WhenFail _ a) = Right a
failToEither (DidFail e) = Left e

(<<$$>>) :: (Bifunctor m) => (a -> b) -> m a a -> m b b
(<<$$>>) f = bimap f f

-- | A pair of read-capability and write-capability.  'Caps' should
-- have a 'Meet' instance, but this would require a "join" definition
-- for @c@, distinct from the multiplicative conjunction meaning of
-- '<>' on capabilities.
data Caps c
  = Caps { capsRead :: c
         , capsWrite :: c
         }

instance (Meet c, Semigroup c) => Semigroup (Caps c) where
  Caps r1 w1 <> Caps r2 w2 = Caps (r1 `meet` r2) (w1 <> w2)

instance (BMeet c, Monoid c) => Monoid (Caps c) where
  mempty = emptyCaps

instance Functor Caps where
  fmap f (Caps rc wc) = Caps (f rc) (f wc)

instance Applicative Caps where
  pure a = Caps a a
  Caps rf wf <*> Caps ra wa = Caps (rf ra) (wf wa)

-- | An empty capability pair, allowing any concurrent update by
-- remote operations and allowing no local update.  This is a synonym
-- for 'mempty'.
emptyCaps :: (BMeet c, Monoid c) => Caps c
emptyCaps = Caps uniC idC

-- | A full capability pair, allowing no concurrent updates (a
-- fully-consistent read) and allowing any local update.  This would
-- be the same as 'meetId' if 'Meet' was implemented for 'Caps'.
fullCaps :: (BMeet c, Monoid c) => Caps c
fullCaps = Caps idC uniC

{-| Check if value is 'idC' by partial-order comparison. -}
isId :: (Monoid c, Meet c) => c -> Bool
isId c = c <=? idC

{-| Check if value is 'uniC' by partial-order comparison. -}
isUni :: (BMeet c) => c -> Bool
isUni c = uniC <=? c

leqCaps :: (Meet c) => Caps c -> Caps c -> Bool
leqCaps (Caps r1 w1) (Caps r2 w2) = r2 <=? r1 && w1 <=? w2
