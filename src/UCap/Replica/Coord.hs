{-# LANGUAGE TemplateHaskell #-}

module UCap.Replica.Coord
  ( Coord
  , requestLock
  , ownsLock
  , Lockconf
  , lockconf
  ) where

import UCap.Domain
import UCap.Lens

data Lockconf i
  = Lockconf { _lcOwner :: Maybe i
             , _lcGen :: Int
             , _lcQueue :: [i]
             }
  deriving (Show,Eq,Ord)

makeLenses ''Lockconf

instance (Ord i) => Semigroup (Lockconf i) where
  Lockconf o1 g1 q1 <> Lockconf o2 g2 q2 =
    let o' = case (o1,o2) of
               (Just i1, _) | g1 >= g2 -> Just i1
               (_, Just i2) | g1 < g2 -> Just i2
               _ -> Nothing
        g' = max g1 g2
        q' = fr q1 q2
        fr (i1:q1) (i2:q2) | i1 == i2 = i1 : fr q1 q2
                           | i1 < i2 = i1 : fr q1 (i2:q2)
                           | i1 > i2 = i2 : fr (i1:q1) q2
    in Lockconf o1 g1 q1

instance (Ord i) => Monoid (Lockconf i) where
  mempty = Lockconf Nothing 0 []

data Coord i c =
  Coord { _lockconf :: Lockconf i
        }
  deriving (Show,Eq,Ord)

makeLenses ''Coord

instance (Ord i, Cap c) => Semigroup (Coord i c) where
  Coord lc1 <> Coord lc2 = Coord (lc1 <> lc2)

instance (Ord i, Cap c) => Monoid (Coord i c) where
  mempty = Coord mempty

requestLock :: i -> Coord i c -> Coord i c
requestLock = undefined

ownsLock :: i -> Coord i c -> Bool
ownsLock = undefined
