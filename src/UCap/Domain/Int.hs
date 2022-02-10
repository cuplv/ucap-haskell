{-# LANGUAGE TypeFamilies #-}

module UCap.Domain.Int where

import Data.InfNum
import UCap.Domain.Classes

class IntOffset a where
  intOffset :: a -> Int

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
subIntE = SubE . decE

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
  type CEffect IncC = IntE
  mincap = incC . intOffset

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

data IntC
  = IntC { addBnd :: IncC
         , subBnd :: DecC
         }
  deriving (Show,Eq,Ord)
