{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module UCap.Domain.Num
  ( CounterE
  , addE
  , subE
  , mulE
  , isAddE
  , isSubE
  , CounterC
  , addC
  , addAny
  , subC
  , subAny
  , mulC
  , mulAny
  , Bounds
  , MulAdd
  , divup
  ) where

import Data.Aeson
import GHC.Generics

import Data.InfNum
import UCap.Domain.Classes
import UCap.Domain.Const

{-| 'CounterE' provides addition, subtraction, and (positive)
  multiplication effects on 'Integral' values.
-}
type CounterE n = ConstE' (MulAdd n)

data MulAdd n
  = MulAdd { mulAmt :: n
           , addAmt :: n
           }
  deriving (Show,Eq,Ord,Generic)

instance (ToJSON n) => ToJSON (MulAdd n) where
  toEncoding = genericToEncoding defaultOptions
instance (FromJSON n) => FromJSON (MulAdd n)

instance (Integral n) => Semigroup (MulAdd n) where
  MulAdd m1 a1 <> MulAdd m2 a2 =
    -- Distributing multiplication over addition.
    MulAdd (m1 * m2) (a1 * m2 + a2)

instance (Integral n) => Monoid (MulAdd n) where
  mempty = MulAdd 1 0

instance (Integral n) => EffectDom (MulAdd n) where
  type EDState (MulAdd n) = n
  eFun (MulAdd m a) s = s * m + a

{-| Add @n@ to the state, where @n >= 0@.  A negative @n@ will produce a
  generate a runtime error.

@
'eFun' ('addE' 1) 2 = 3

'eFun' ('addE' 0) = 'Data.Function.id'
@
-}
addE :: (Ord n, Integral n) => n -> CounterE n
addE n | n >= 0 = ModifyE (MulAdd mulId n)
       | otherwise = error $ "Negative value applied to addE."

{-| Subtract @n@ from the state, where @n >= 0@.

@
'eFun' ('subE' 1) 3 = 2

'eFun' ('subE' 0) = 'Data.Function.id'
@
-}
subE :: (Ord n, Integral n) => n -> CounterE n
subE n | n >= 0 = ModifyE (MulAdd mulId (-n))
       | otherwise = error $ "Negative value applied to subE."

{-| Multiply the state by @n@, where @n >= 0@.

@
'eFun' ('mulE' 2) 3 = 6

'eFun' ('mulE' 1) = 'Data.Function.id'
@
-}
mulE :: (Ord n, Integral n) => n -> CounterE n
mulE n | n >= 0 = ModifyE (MulAdd n addId)
       | otherwise = error $ "Negative value applied to mulE."

isAddE :: (Ord n, Integral n) => CounterE n -> Bool
isAddE (ModifyE (MulAdd m a)) = m == mulId && a > addId
isAddE _ = False

isSubE :: (Ord n, Integral n) => CounterE n -> Bool
isSubE (ModifyE (MulAdd m a)) = m == mulId && a < addId
isSubE _ = False

{-| Check that an 'EMulAdd' effect only adds/subtracts, and does not
    multiply.

@
'additive' ('addE' 1) = 'True'

'additive' ('mulE' 2 'Data.Semigroup.<>' 'addE' 1) = 'False'

'additive' ('mulE' 1 'Data.Semigroup.<>' 'addE' 1) = 'True'
@
-}
additive :: (Eq n, Integral n) => MulAdd n -> Bool
additive (MulAdd m _) = m == mulId

data Bounds n
  = Bounds { addBound :: AddBound n
           , subBound :: AddBound n
           , mulBound :: MulBound n
           }
  deriving (Eq,Ord,Generic)

instance (Ord n, Integral n, Show n) => Show (Bounds n) where
  show (Bounds a s m) = 
    "["
    ++ (if not $ a <=? mempty
           then "+" ++ show a ++ ","
           else "")
    ++ (if not $ s <=? mempty
           then "-" ++ show s ++ ","
           else "")
    ++ (if not $ m <=? mempty
           then "×" ++ show m ++ ","
           else "")
    ++ "]"

instance (ToJSON n) => ToJSON (Bounds n) where
  toEncoding = genericToEncoding defaultOptions
instance (ToJSON n, ToJSONKey n) => ToJSONKey (Bounds n)
instance (FromJSON n) => FromJSON (Bounds n)
instance (FromJSON n, FromJSONKey n) => FromJSONKey (Bounds n)

addC' :: (Integral n, Ord n) => n -> Bounds n
addC' n = Bounds (addB n) mempty mempty

subC' :: (Integral n, Ord n) => n -> Bounds n
subC' n = Bounds mempty (addB n) mempty

mulC' :: (Integral n, Ord n) => n -> Bounds n
mulC' n = Bounds mempty mempty (mulB n)

addAny' :: (Integral n, Ord n) => Bounds n
addAny' = Bounds meetId mempty mempty

subAny' :: (Integral n, Ord n) => Bounds n
subAny' = Bounds mempty meetId mempty

mulAny' :: (Integral n, Ord n) => Bounds n
mulAny' = Bounds mempty mempty meetId

instance (Integral n) => Semigroup (Bounds n) where
  Bounds a1 s1 m1 <> Bounds a2 s2 m2 =
    Bounds (a1 <> a2) (s1 <> s2) (m1 <> m2)

instance (Integral n) => Monoid (Bounds n) where
  mempty = Bounds mempty mempty mempty

instance (Integral n, Ord n) => Meet (Bounds n) where
  meet (Bounds a1 s1 m1) (Bounds a2 s2 m2) =
    Bounds (a1 `meet` a2) (s1 `meet` s2) (m1 `meet` m2)
  (<=?) (Bounds a1 s1 m1) (Bounds a2 s2 m2) =
    a1 <=? a2 && s1 <=? s2 && m1 <=? m2

instance (Integral n, Ord n) => BMeet (Bounds n) where
  meetId = Bounds meetId meetId meetId

instance (Integral n, Ord n) => Split (Bounds n) where
  split (Bounds a1 s1 m1) (Bounds a2 s2 m2) = failToEither $
    Bounds <<$$>> splitWF a1 a2 <<*>> splitWF s1 s2 <<*>> splitWF m1 m2

instance (Integral n, Ord n) => Cap (Bounds n) where
  type CEffect (Bounds n) = MulAdd n
  mincap e = if addAmt e >= addId
                then Bounds (addB $ addAmt e) mempty (mulB $ mulAmt e)
                else Bounds mempty
                            (addB . negate $ addAmt e)
                            (mulB $ mulAmt e)

  undo e = if addAmt e >= addId
              then Bounds mempty (addB $ addAmt e) mempty
              else Bounds (addB . negate $ addAmt e) mempty mempty

  weaken c1@(Bounds a1 s1 m1) c2@(Bounds a2 s2 m2)
    | uniC <=? c1 || c2 <=? idE = Just idE
    | m2 <=? mempty && meetId <=? a1 = MulAdd mulId <$> addFun s2
    | m2 <=? mempty && meetId <=? s1 =
      MulAdd mulId . negate <$> addFun a2
    | otherwise = Nothing

type CounterC n = ConstC' (Bounds n)

addC :: (Integral n, Ord n) => n -> CounterC n
addC = modifyC . addC'

addAny :: (Integral n, Ord n) => CounterC n
addAny = modifyC addAny'

subC :: (Integral n, Ord n) => n -> CounterC n
subC = modifyC . subC'

subAny :: (Integral n, Ord n) => CounterC n
subAny = modifyC subAny'

mulC :: (Integral n, Ord n) => n -> CounterC n
mulC = modifyC . mulC'

mulAny :: (Integral n, Ord n) => CounterC n
mulAny = modifyC mulAny'

divup :: [String] -> CounterE Int -> [(String,CounterE Int)]
divup ss (ModifyE (MulAdd m a)) | m == mulId =
  let l = length ss
      n = quot a l
  in map (\s -> (s, addE n)) ss
divup (s:_) e = [(s, e)]
divup [] _ = []
