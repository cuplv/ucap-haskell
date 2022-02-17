{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.SRQueue
  ( -- * Multi-writer queue
    MWQueue
  , mwEmpty
  , mwEnqueue
  , mwEnqueueAll
  , mwDequeue
  , mwDequeueAll
  , mwPeek
  , mwPeekAll
  , mwLength
    -- * Single-reader queue
  , SRQueue
  , srEmpty
  , srInit
  , srEnqueue
  , srEnqueueAll
  , srDequeue
  , srDequeueAll
  , srPeek
  , srPeekAll
  , srLength
    -- * Single-editor cell
  , SECell
  , seInit
  , seSet
  , seGet
  , seMod
  ) where

import UCap.Lens

import Data.Aeson
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics


data SRQueue a
  = SRQueue { lqConsumed :: Int
            , lqElements :: [a]
            }
  deriving (Eq,Ord,Generic)

instance (Show a) => Show (SRQueue a) where
  show (SRQueue 0 es) = "SRQueue(" ++ show es ++ ")"
  show (SRQueue n es) = "SRQueue(" ++ show n ++ " + " ++ show es ++ ")"

instance (ToJSON a) => ToJSON (SRQueue a)
instance (FromJSON a) => FromJSON (SRQueue a)

instance (Ord a) => Semigroup (SRQueue a) where
  SRQueue g1 as1 <> SRQueue g2 as2 =
    let fr [] as2 = as2
        fr as1 [] = as1
        fr (a1:as1) (a2:as2) | a1 == a2 = a1 : fr as1 as2
                             | a1 < a2 = a1 : fr as1 (a2:as2)
                             | a1 > a2 = a2 : fr (a1:as1) as2
        g' = max g1 g2
    in SRQueue g' $ fr (drop (g' - g1) as1) (drop (g' - g2) as2)

instance (Ord a) => Monoid (SRQueue a) where
  mempty = srEmpty

{-| An 'SRQueue' containing no elements. -}
srEmpty :: SRQueue a
srEmpty = srInit []

{-| An 'SRQueue' containing only the given elements.

@
'srPeek' ('srInit' [1,2,3]) '==' 'Just' 1
@
-}
srInit :: [a] -> SRQueue a
srInit as = SRQueue 0 as

{-| Add an element to the queue. -}
srEnqueue :: a -> SRQueue a -> SRQueue a
srEnqueue a = srEnqueueAll [a]

{-| Enqueue a list of elements.

@
('srEnqueue' 2 . 'srEnqueue' 1 $ q) '==' 'srEnqueueAll' [1,2] q
@
-}
srEnqueueAll :: [a] -> SRQueue a -> SRQueue a
srEnqueueAll as' (SRQueue g as) = SRQueue g (as ++ as')

{-| Get the first element, if available. -}
srPeek :: SRQueue a -> Maybe a
srPeek q = snd <$> srDequeue q

{-| Get all the elements.

@
('srPeekAll' . 'srEnqueue' 2 . 'srEnqueue' 1 $ 'srEmpty') '==' [1,2]
@
-}
srPeekAll :: SRQueue a -> [a]
srPeekAll q = snd $ srDequeueAll q

{-| Pop the next element from the queue (if it exists).  This must not
  be performed concurrently with other 'srDequeue' or 'srDequeueAll'
  changes. -}
srDequeue :: SRQueue a -> Maybe (SRQueue a, a)
srDequeue (SRQueue g (a:as)) = Just (SRQueue (g + 1) as, a)
srDequeue _ = Nothing

{-| Consume the entire queue as a list.  Just like 'srDequeue', this
  must not be performed concurrently to other 'srDequeue' or
  'srDequeueAll' changes. -}
srDequeueAll :: SRQueue a -> (SRQueue a, [a])
srDequeueAll (SRQueue g as) = (SRQueue (g + length as) [], as)

{-| Get the length of the queue. -}
srLength :: SRQueue a -> Int
srLength (SRQueue _ as) = length as

srIsInitial :: SRQueue a -> Bool
srIsInitial (SRQueue 0 []) = True
srIsInitial _ = False

{-| An 'MWQueue' is a queue of elements that can be safely added-to in a
  concurrent way.  This means that two added-to queues can be derived
  from a common ancestor and then merged ('<>'), and no elements will
  be lost or duplicated.

@
let q = 'mwEmpty'
    q3 = 'srEnqueueAll' i1 [3,0,5] q
    q4 = 'srEnqueue' i2 4 q
in  'srPeekAll' (q3 '<>' q4) == [1,2,3,0,4,5]
    '&&' 'srPeekAll' (q4 '<>' q3) == [1,2,3,0,4,5]
@

  The order of concurrent elements is determined by their 'Ord'
  instance.
-}
data MWQueue i a
  = MWQueue { _mwQueue :: Map i (SRQueue a) }
  deriving (Eq,Ord,Generic)

makeLenses ''MWQueue

instance (Ord i, Show a) => Show (MWQueue i a) where
  show q = "MWQueue(" ++ show (mwPeekAll q) ++ ")"

instance (ToJSON i, ToJSONKey i, ToJSON a) => ToJSON (MWQueue i a)
instance (Ord i, FromJSON i, FromJSONKey i, FromJSON a) => FromJSON (MWQueue i a)

instance (Ord i, Ord a) => Semigroup (MWQueue i a) where
  MWQueue m1 <> MWQueue m2 = MWQueue $ Map.unionWith (<>) m1 m2

mwEmpty :: MWQueue i a
mwEmpty = MWQueue Map.empty

srl :: (Ord i) => i -> Lens' (MWQueue i a) (SRQueue a)
srl i = mwQueue . at i . nani srEmpty srIsInitial

mwEnqueue :: (Ord i) => i -> a -> MWQueue i a -> MWQueue i a
mwEnqueue i a = srl i %~ srEnqueue a

mwEnqueueAll :: (Ord i) => i -> [a] -> MWQueue i a -> MWQueue i a
mwEnqueueAll i as = srl i %~ srEnqueueAll as

mwDequeue :: (Ord i) => MWQueue i a -> Maybe (MWQueue i a, a)
mwDequeue q@(MWQueue m) =
  let ks = Map.keys m
      comp i1 i2 = 
        case compare (srLength $ q ^. srl i2) (srLength $ q ^. srl i1) of
          EQ -> compare i1 i2
          o -> o
  in case List.sortBy comp ks of
       i : _ -> case srDequeue $ q ^. srl i of
                  Just (iq',a) -> Just (q & srl i .~ iq', a)
                  Nothing -> Nothing
       [] -> Nothing

mwDequeueAll :: (Ord i) => MWQueue i a -> (MWQueue i a, [a])
mwDequeueAll q = case mwDequeue q of
                   Just (q',a) -> (a :) <$> mwDequeueAll q'
                   Nothing -> (q,[])

mwPeek :: (Ord i) => MWQueue i a -> Maybe a
mwPeek q = snd <$> mwDequeue q

mwPeekAll :: (Ord i) => MWQueue i a -> [a]
mwPeekAll q = snd $ mwDequeueAll q

mwLength :: (Ord i) => MWQueue i a -> Int
mwLength = length . mwPeekAll


{-| An 'SECell' is a versioned record.  As long as new versions are not
  created concurrently, two related 'SECell's will merge to the newest
  version. -}
data SECell a
  = SECell Int a
  deriving (Eq,Ord,Generic)

instance (Show a) => Show (SECell a) where
  show (SECell 0 a) = "SECell(" ++ show a ++ ")"
  show (SECell n a) = "SECell(" ++ show n ++ " = " ++ show a ++ ")"

instance (ToJSON a) => ToJSON (SECell a)
instance (FromJSON a) => FromJSON (SECell a)

instance (Eq a) => Semigroup (SECell a) where
  SECell g1 a1 <> SECell g2 a2
    | g1 > g2 = SECell g1 a1
    | g1 < g2 = SECell g2 a2
    | a1 == a2 = SECell g1 a1
    | otherwise = error "Conflicting cell values."

seInit :: a -> SECell a
seInit = SECell 0

{-| Set a new cell value.  This must not be concurrent to any other
  'seSet' or 'seMod'.

@
'seGet' ('seInit' b) '==' 'seGet' ('seSet' b $ 'seInit' a)
@

If the new value is the same as the old, the 'SECell' is not changed
and will still be '==' to the old 'SECell'.

@
'seInit' a '==' 'seSet' a ('seInit' a)
@
-}
seSet :: (Eq a) => a -> SECell a -> SECell a
seSet a s@(SECell g a0) | a /= a0 = SECell (g + 1) a
                        | otherwise = s

{-|
@
'seGet' ('seInit' 3) '==' 3
@
-}
seGet :: SECell a -> a
seGet (SECell _ a) = a

{-| Change the cell value by modifying the current value with a
  function.  This must not be concurrent to any other 'seSet' or
  'seMod'.  If the new value is the same as the old, the 'SECell' is
  not changed and will still be '==' to the old 'SECell'. -}
seMod :: (Eq a) => (a -> a) -> SECell a -> SECell a
seMod f c = seSet (f $ seGet c) c
