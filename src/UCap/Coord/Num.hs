{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module UCap.Coord.Num where

import Data.InfNum
import UCap.Coord.Classes
import UCap.Domain.Classes
import UCap.Domain.Num
import UCap.Lens

import Data.Bifunctor

class IntCoord g where
  consumeIC :: (Ord i) => i -> Int -> g i -> Either Int (g i)
  produceIC :: (Ord i) => i -> Int -> g i -> g i
  safeUnder :: (Ord i) => i -> g i -> Maybe Int
  requestFor :: (Ord i) => i -> Int -> g i -> Maybe (g i)
  handleReqs :: (Ord i) => i -> g i -> g i
  localIntWrite :: (Ord i) => i -> g i -> AddBound Int
  localIntRead :: (Ord i) => i -> g i -> AddBound Int

instance IntCoord Token where
  consumeIC i n t | n == 0 = Right t
                  | i == tokenOwner t = Right t
                  | otherwise = Left n
  produceIC _ _ = id
  safeUnder i t | i == tokenOwner t = Just 0
                | otherwise = Nothing
  requestFor i _ = Just . requestToken i
  handleReqs = handleTokenReqs
  localIntWrite i t | i == tokenOwner t = meetId
                    | otherwise = mempty
  localIntRead i t | i == tokenOwner t = mempty
                   | otherwise = meetId

instance IntCoord EscrowIntPool where
  consumeIC = escrowUse
  produceIC = escrowAdd
  safeUnder i p = Just $ escrowUnowned i p
  requestFor i n p = case p ^. epSources of
    i1 : _ -> Just $ escrowRequest i (i1,n) p
    [] -> Nothing
  handleReqs = escrowHandleReqs
  localIntWrite i p = addB $ escrowOwned i p
  localIntRead i p = addB $ escrowUnowned i p

data CounterG i g1 g2
  = CounterG { _addCoord :: g1 i, _subCoord :: g2 i }
  deriving (Show,Eq,Ord)

makeLenses ''CounterG

instance (Ord i, IntCoord g1, IntCoord g2)
  => CoordSys (CounterG i g1 g2) where
  type GCap (CounterG i g1 g2) = Bounds Int
  type GId (CounterG i g1 g2) = i
  resolveCaps i (Bounds ab sb mb) = undefined
  resolveEffect i (MulAdd m n) g
    | m == mulId && n == 0 = Right g
    | m == mulId && n > 0 = bimap
       addC'
       (\ac -> g & addCoord .~ ac
                 & subCoord %~ produceIC i n)
       (consumeIC i n (g^.addCoord))
    | m == mulId && n < 0 = bimap
       subC'
       (\sc -> g & subCoord .~ sc
                 & addCoord %~ produceIC i n)
       (consumeIC i n (g^.subCoord))

  localCaps i g =
    let rc = Bounds { addBound = localIntRead i (g^.addCoord)
                    , subBound = localIntRead i (g^.subCoord)
                    , mulBound = mempty
                    }
        wc = Bounds { addBound = localIntWrite i (g^.addCoord)
                    , subBound = localIntWrite i (g^.subCoord)
                    , mulBound = mempty
                    }
    in Caps { capsRead = rc, capsWrite = wc }
