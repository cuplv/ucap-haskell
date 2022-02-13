module Data.SRQueue
  ( SRQueue
  , srEmpty
  , srInit
  , srEnqueue
  , srEnqueueAll
  , srDequeue
  , srDequeueAll
  , srLength
  , SECell
  , seInit
  , seSet
  , seGet
  , seMod
  ) where

{-| An 'SRQueue' is a queue of elements that can be safely added-to in a
  concurrent way.  This means that two added-to queues can be derived
  from a common ancestor and then merged ('<>'), and no elements will
  be lost or duplicated.

@
let q = 'srInit' [1,2]
    q3 = 'srEnqueueAll' [3,0,5] q
    q4 = 'srEnqueue' 4 q
in  'srDequeueAll' (q3 '<>' q4) == [1,2,3,0,4,5]
    '&&' 'srDequeueAll' (q4 '<>' q3) == [1,2,3,0,4,5]
@

  The order of concurrent elements is determined by their 'Ord'
  instance.
-}
data SRQueue a
  = SRQueue { lqConsumed :: Int
            , lqElements :: [a]
            }
  deriving (Show,Eq,Ord)

instance (Ord a) => Semigroup (SRQueue a) where
  SRQueue g1 as1 <> SRQueue g2 as2 =
    let fr [] as2 = as2
        fr as1 [] = as1
        fr (a1:as1) (a2:as2) | a1 == a2 = a1 : fr as1 as2
                             | a1 > a2 = a1 : fr as1 (a2:as2)
                             | a1 < a2 = a2 : fr (a1:as1) as2
        g' = max g1 g2
    in SRQueue g' $ fr (drop g' as1) (drop g' as2)

instance (Ord a) => Monoid (SRQueue a) where
  mempty = srEmpty

{-| An 'SRQueue' containing no elements. -}
srEmpty :: SRQueue a
srEmpty = srInit []

{-| An 'SRQueue' containing only the given elements. -}
srInit :: [a] -> SRQueue a
srInit as = SRQueue 0 as

{-| Add an element to the queue.  This is safe to perform concurrently with other 'srEnqueue' changes. -}
srEnqueue :: a -> SRQueue a -> SRQueue a
srEnqueue a = srEnqueueAll [a]

srEnqueueAll :: [a] -> SRQueue a -> SRQueue a
srEnqueueAll as1 (SRQueue g as2) = SRQueue g (as1 ++ as2)

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

data SECell a
  = SECell Int a
  deriving (Show,Eq,Ord)

instance (Eq a) => Semigroup (SECell a) where
  SECell g1 a1 <> SECell g2 a2
    | g1 > g2 = SECell g1 a1
    | g1 < g2 = SECell g2 a2
    | a1 == a2 = SECell g1 a1
    | otherwise = error "Conflicting cell values."

seInit :: a -> SECell a
seInit = SECell 0

seSet :: a -> SECell a -> SECell a
seSet a (SECell g _) = SECell (g + 1) a

seGet :: SECell a -> a
seGet (SECell _ a) = a

seMod :: (a -> a) -> SECell a -> SECell a
seMod f c = seSet (f $ seGet c) c
