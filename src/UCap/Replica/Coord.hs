{-# LANGUAGE TemplateHaskell #-}

module UCap.Replica.Coord
  ( Coord
  , withLock
  , requestLock
  , isRequestedOf
  , grantReq
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

requestLock :: (Ord i) => i -> Coord i c -> Coord i c
requestLock i1 cd@(Coord (Lockconf (Just i2) gen q))
  | i1 == i2 = cd
  | i1 `elem` q = cd
  | otherwise = Coord (Lockconf (Just i2) gen (q ++ [i1]))
requestLock _ _ = error "Can't request lock when not owned."

ownsLock :: (Eq i) => i -> Coord i c -> Bool
ownsLock i1 (Coord (Lockconf (Just i2) _ _)) = i1 == i2
ownsLock _ _ = False

isRequestedOf :: (Eq i) => i -> Coord i c -> Bool
isRequestedOf i1 (Coord (Lockconf (Just i2) _ q)) = i1 == i2 && not (null q)
isRequestedOf _ _ = False

grantReq :: (Eq i) => i -> Coord i c -> Coord i c
grantReq i1 (Coord (Lockconf (Just i2) gen (i3:is)))
  | i1 == i2 = Coord (Lockconf (Just i3) (gen + 1) is)
  | otherwise = error "Non-owner tried to grant."
grantReq _ _ = error "Grant error."

{-| Initialize the 'Coord' structure with the lock held. -}
withLock :: i -> Coord i c
withLock i = Coord (Lockconf (Just i) 0 [])
