{-# LANGUAGE TypeFamilies #-}

module UCap.Domain.Int where

import Data.InfNum
import UCap.Domain.Classes

class IntOffset a where
  intOffset :: a -> Int

class (Cap c) => MaxEffect c where
  maxEffect :: c -> Maybe (CEffect c)

newtype IncE = IncE Int deriving (Show,Eq,Ord)

instance Semigroup IncE where
  IncE n1 <> IncE n2 = IncE (n1 + n2)

instance Monoid IncE where
  mempty = incE 0

instance IntOffset IncE where
  intOffset (IncE n) = n

instance Meet IncE where
  a <=? b = intOffset a <= intOffset b
  a `meet` b = incE $ min (intOffset a) (intOffset b)

instance EffectDom IncE where
  type EDState IncE = Int
  eFun a s = s + (intOffset a)

incE :: Int -> IncE
incE n | n >= 0 = IncE n
       | otherwise = error "Int arg must be >= 0."

newtype DecE = DecE IncE deriving (Show,Eq,Ord)

instance Semigroup DecE where
  DecE a <> DecE b = DecE (a <> b)

instance Monoid DecE where
  mempty = decE 0

instance IntOffset DecE where
  intOffset (DecE a) = (- intOffset a)

instance Meet DecE where
  a <=? b = intOffset a <= intOffset b
  a `meet` b = decE $ min (intOffset a) (intOffset b)

instance EffectDom DecE where
  type EDState DecE = Int
  eFun a s = s + (intOffset a)

decE :: Int -> DecE
decE = DecE . incE

data IntE = AddE IncE | SubE DecE deriving (Show)

instance Eq IntE where
  a == b = intOffset a == intOffset b

instance Ord IntE where
  a <= b = intOffset a <= intOffset b

instance Semigroup IntE where
  a <> b = intE $ intOffset a + intOffset b

instance Monoid IntE where
  mempty = intE 0

instance IntOffset IntE where
  intOffset (AddE i) = intOffset i
  intOffset (SubE d) = intOffset d

instance EffectDom IntE where
  type EDState IntE = Int
  eFun a s = s + (intOffset a)

intE :: Int -> IntE
intE n | n >= 0 = addIntE n -- Canonical 0 is (AddE 0), but Eq and Ord
                            -- do not differentiate (AddE 0) from
                            -- (SubE 0).
intE n | n < 0 = addIntE (-n)

addIntE :: Int -> IntE
addIntE = AddE . incE

subIntE :: Int -> IntE
subIntE 0 = mempty -- AddE 0 instead of SubE 0
subIntE n = SubE $ decE n

invertIntE :: IntE -> IntE
invertIntE (AddE e) = SubE (DecE e)
invertIntE (SubE (DecE e)) = AddE e

newtype IncC = IncC (AddBound Int) deriving (Show,Eq,Ord)

incC :: Int -> IncC
incC = IncC . addB

incInfC :: IncC
incInfC = IncC meetId

instance Semigroup IncC where
  IncC a <> IncC b = IncC $ a <> b

instance Monoid IncC where
  mempty = IncC mempty

instance Meet IncC where
  IncC a <=? IncC b = a <=? b
  IncC a `meet` IncC b = IncC $ a `meet` b

instance BMeet IncC where
  meetId = IncC meetId

instance Split IncC where
  split (IncC a) (IncC b) = IncC <<$$>> split a b

instance Cap IncC where
  type CEffect IncC = IncE
  mincap = incC . intOffset

instance MaxEffect IncC where
  maxEffect (IncC a) = incE <$> addFun a

newtype DecC = DecC IncC deriving (Show,Eq,Ord)

decC :: Int -> DecC
decC = DecC . incC

decInfC :: DecC
decInfC = DecC incInfC

instance Semigroup DecC where
  DecC a <> DecC b = DecC $ a <> b

instance Monoid DecC where
  mempty = DecC mempty

instance Meet DecC where
  DecC a <=? DecC b = a <=? b
  DecC a `meet` DecC b = DecC $ a `meet` b

instance BMeet DecC where
  meetId = DecC meetId

instance Split DecC where
  split (DecC a) (DecC b) = DecC <<$$>> split a b

instance Cap DecC where
  type CEffect DecC = DecE
  mincap = decC . intOffset

instance MaxEffect DecC where
  maxEffect (DecC c) = DecE <$> maxEffect c

data IntC
  = IntC { addBnd :: IncC
         , subBnd :: DecC
         }
  deriving (Show,Eq,Ord)

intC :: Int -> IntC
intC n | n >= 0 = addIntC n
       | n < 0 = subIntC (-n)

addIntC :: Int -> IntC
addIntC n = IntC (incC n) mempty

fromIncC :: IncC -> IntC
fromIncC a = IntC a mempty

subIntC :: Int -> IntC
subIntC 0 = mempty
subIntC n = IntC mempty (decC n)

fromDecC :: DecC -> IntC
fromDecC a | a == mempty = mempty
           | otherwise = IntC mempty a

instance Semigroup IntC where
  IntC a1 s1 <> IntC a2 s2 = IntC (a1 <> a2) (s1 <> s2)

instance Monoid IntC where
  mempty = IntC mempty mempty

instance Meet IntC where
  IntC a1 s1 <=? IntC a2 s2 = a1 <=? a2 && s1 <=? s2
  IntC a1 s1 `meet` IntC a2 s2 = IntC (meet a1 a2) (meet s1 s2)

instance BMeet IntC where
  meetId = IntC meetId meetId

instance Split IntC where
  split (IntC a1 s1) (IntC a2 s2) = failToEither $
    IntC <<$$>> splitWF a1 a2 <<*>> splitWF s1 s2

instance Cap IntC where
  type CEffect IntC = IntE
  mincap (AddE e) = IntC (mincap e) idC
  mincap (SubE e) = IntC idC (mincap e)
  undo = mincap . invertIntE
  weaken (IntC a1 s1) (IntC a2 s2)
    | isUni a1 && isUni s1 = Just idE
    | isUni a1 = SubE <$> maxEffect s2
    | isUni s1 = AddE <$> maxEffect a2
    | otherwise = Nothing
