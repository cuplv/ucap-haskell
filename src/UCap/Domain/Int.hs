{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module UCap.Domain.Int where

import Data.InfNum
import UCap.Domain.Classes

import Data.Aeson
import GHC.Generics

class IntOffset a where
  intOffset :: a -> Int

class (Cap c) => MaxEffect c where
  maxEffect :: c -> Maybe (CEffect c)

newtype IncE = IncE Int deriving (Show,Eq,Ord,Generic)

instance ToJSON IncE
instance FromJSON IncE

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

newtype DecE = DecE IncE deriving (Show,Eq,Ord,Generic)

instance ToJSON DecE
instance FromJSON DecE

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

data IntE = AddE IncE | SubE DecE deriving (Show,Generic)

instance ToJSON IntE
instance FromJSON IntE

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
intE n | n >= 0 = addE n -- Canonical 0 is (AddE 0), but Eq and Ord
                         -- do not differentiate (AddE 0) from
                         -- (SubE 0).
intE n | n < 0 = subE (-n)

addE :: Int -> IntE
addE = AddE . incE

subE :: Int -> IntE
subE 0 = mempty -- AddE 0 instead of SubE 0
subE n = SubE $ decE n

invertIntE :: IntE -> IntE
invertIntE (AddE e) = SubE (DecE e)
invertIntE (SubE (DecE e)) = AddE e

newtype IncC = IncC (AddBound Int) deriving (Show,Eq,Ord,Generic)

instance ToJSON IncC
instance FromJSON IncC

incC :: Int -> IncC
incC n | n >= 0 = IncC . addB $ n
       | otherwise = error $ "Oops, " ++ show n ++ " is < 0."

incInfC :: IncC
incInfC = IncC meetId

incBound :: IncC -> Maybe Int
incBound (IncC b) = addFun b

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

newtype DecC = DecC { unwrapDecC :: IncC } deriving (Show,Eq,Ord,Generic)

instance ToJSON DecC
instance FromJSON DecC

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
  mincap (DecE (IncE n)) = decC n

instance MaxEffect DecC where
  maxEffect (DecC c) = DecE <$> maxEffect c

data IntC
  = IntC { addBnd :: IncC
         , subBnd :: DecC
         }
  deriving (Show,Eq,Ord,Generic)

instance ToJSON IntC
instance FromJSON IntC

intC :: Int -> IntC
intC n | n >= 0 = addC n
       | n < 0 = subC (-n)

addC :: Int -> IntC
addC n = IntC (incC n) mempty

fromIncC :: IncC -> IntC
fromIncC a = IntC a mempty

addAny :: IntC
addAny = fromIncC incInfC

{-| Cap allowing all additions and no subtractions.  This is a synonym
  for 'addAny'. -}
lowerBoundC :: IntC
lowerBoundC = addAny

subC :: Int -> IntC
subC 0 = mempty
subC n = IntC mempty (decC n)

fromDecC :: DecC -> IntC
fromDecC a | a == mempty = mempty
           | otherwise = IntC mempty a

subAny :: IntC
subAny = fromDecC decInfC

{-| Cap allowing all subtractions and no additions.  This is a synonym
  for 'subAny'. -}
upperBoundC :: IntC
upperBoundC = subAny

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
