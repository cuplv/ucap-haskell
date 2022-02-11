{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module UCap.Coord.Classes where

import Data.SRQueue
import UCap.Domain.Classes
import UCap.Lens

import Data.Map (Map)
import qualified Data.Map as Map

type family GEffect g where
  GEffect g = CEffect (GCap g)

type family GState g where
  GState g = CState (GCap g)

class (Cap (GCap g)) => CoordSys g where

  {-| The type of capabilities (which also defines the state and effect
  types). -}
  type GCap g

  {-| The type of replica IDs. -}
  type GId g

  {-| For given capability requirements, check if they are satisifed by
    the coordination system.  If so, return @'Right' f@, where @f@ is
    a simulation function that modifies the observed state value.  If
    not, attempt to perform requests that will eventually satisfy
    those requirements.  If this is possible, return the
    request-containing system in @'Left' ('Just' y)@.  If not, and
    thus there is no way to satisfy the requirements, return @'Left'
    'Nothing'@. -}
  resolveCaps :: GId g -> Caps (GCap g) -> g -> Either (Maybe g) (GEffect g)
  {-| For given effect, modify the coordination system to reflect its use.
    If the coordination system locally permits issuing the effect,
    then modify it accordingly and return @'Right' y@.  If not, return
    @'Left' c@, indicating a write failure with excess write
    capability @c@. -}
  resolveEffect :: GId g -> GEffect g -> g -> Either (GCap g) g

  {-| The locally-held read and write capabilities. -}
  localCaps :: GId g -> g -> Caps (GCap g)

  undoEffect :: GId g -> GEffect g -> g -> g
  undoEffect _ _ = id

  grantRequests :: GId g -> g -> g
  grantRequests _ = id

la2 f a b = f <$> a <*> b

la3 f a b c = f <$> a <*> b <*> c

la4 f a b c d = f <$> a <*> b <*> c <*> d

instance (GId a ~ GId b, CoordSys a, CoordSys b) => CoordSys (a,b) where
  type GCap (a,b) = (GCap a, GCap b)
  type GId (a,b) = GId a
  resolveCaps i cs (a,b) = failToEither $ WhenFail (la2 (,)) (,)
    <<*>> (eitherToWF (pure a) $ resolveCaps i (fst <$> cs) a)
    <<*>> (eitherToWF (pure b) $ resolveCaps i (snd <$> cs) b)
  resolveEffect i (ea,eb) (a,b) = failToEither $ WhenFail (,) (,)
    <<*>> (eitherToWF idC $ resolveEffect i ea a)
    <<*>> (eitherToWF idC $ resolveEffect i eb b)
  localCaps i (a,b) = (,) <$> localCaps i a <*> localCaps i b
  undoEffect i (ea,eb) (a,b) = (undoEffect i ea a, undoEffect i eb b)
  grantRequests i (a,b) = (grantRequests i a, grantRequests i b)

instance (GId a ~ GId b, GId a ~ GId c, CoordSys a, CoordSys b, CoordSys c) => CoordSys (a,b,c) where
  type GCap (a,b,c) = (GCap a, GCap b, GCap c)
  type GId (a,b,c) = GId a
  resolveCaps i cs (a,b,c) = failToEither $ WhenFail (la3 (,,)) (,,)
    <<*>> (eitherToWF (pure a) $ resolveCaps i (view _1 <$> cs) a)
    <<*>> (eitherToWF (pure b) $ resolveCaps i (view _2 <$> cs) b)
    <<*>> (eitherToWF (pure c) $ resolveCaps i (view _3 <$> cs) c)
  resolveEffect i (ea,eb,ec) (a,b,c) = failToEither $ WhenFail (,,) (,,)
    <<*>> (eitherToWF idC $ resolveEffect i ea a)
    <<*>> (eitherToWF idC $ resolveEffect i eb b)
    <<*>> (eitherToWF idC $ resolveEffect i ec c)
  localCaps i (a,b,c) = (,,)
    <$> localCaps i a
    <*> localCaps i b
    <*> localCaps i c
  undoEffect i (ea,eb,ec) (a,b,c) = (,,)
    (undoEffect i ea a)
    (undoEffect i eb b)
    (undoEffect i ec c)
  grantRequests i (a,b,c) = (,,)
    (grantRequests i a)
    (grantRequests i b)
    (grantRequests i c)

instance (GId a ~ GId b, GId a ~ GId c, GId a ~ GId d, CoordSys a, CoordSys b, CoordSys c, CoordSys d) => CoordSys (a,b,c,d) where
  type GCap (a,b,c,d) = (GCap a, GCap b, GCap c, GCap d)
  type GId (a,b,c,d) = GId a
  resolveCaps i cs (a,b,c,d) = failToEither $ WhenFail (la4 (,,,)) (,,,)
    <<*>> (eitherToWF (pure a) $ resolveCaps i (view _1 <$> cs) a)
    <<*>> (eitherToWF (pure b) $ resolveCaps i (view _2 <$> cs) b)
    <<*>> (eitherToWF (pure c) $ resolveCaps i (view _3 <$> cs) c)
    <<*>> (eitherToWF (pure d) $ resolveCaps i (view _4 <$> cs) d)
  resolveEffect i (ea,eb,ec,ed) (a,b,c,d) = failToEither $ WhenFail (,,,) (,,,)
    <<*>> (eitherToWF idC $ resolveEffect i ea a)
    <<*>> (eitherToWF idC $ resolveEffect i eb b)
    <<*>> (eitherToWF idC $ resolveEffect i ec c)
    <<*>> (eitherToWF idC $ resolveEffect i ed d)
  localCaps i (a,b,c,d) = (,,,)
    <$> localCaps i a
    <*> localCaps i b
    <*> localCaps i c
    <*> localCaps i d
  undoEffect i (ea,eb,ec,ed) (a,b,c,d) = (,,,)
    (undoEffect i ea a)
    (undoEffect i eb b)
    (undoEffect i ec c)
    (undoEffect i ed d)
  grantRequests i (a,b,c,d) = (,,,)
    (grantRequests i a)
    (grantRequests i b)
    (grantRequests i c)
    (grantRequests i d)

data Token i
  = Token { _tkOwner :: SECell i
          , _tkQueue :: SRQueue i
          }
  deriving (Show,Eq,Ord)

makeLenses ''Token

instance (Ord i) => Semigroup (Token i) where
  Token o1 q1 <> Token o2 q2 = Token (o1 <> o2) (q1 <> q2)

mkToken :: i -> Token i
mkToken i = Token (seInit i) srEmpty

tokenOwner :: Token i -> i
tokenOwner (Token o _) = seGet o

isRequestedOf :: (Eq i) => i -> Token i -> Bool
isRequestedOf i (Token o q) = i == seGet o && srLength q > 0

requestToken :: (Ord i) => i -> Token i -> Token i
requestToken i tk@(Token o q)
  | i == seGet o = tk
  | otherwise = Token o (srEnqueue i q)

data GrantError
  = NotOwner
  | NotRequested
  deriving (Show,Eq,Ord)

grantToken :: (Eq i) => i -> Token i -> Either GrantError (Token i)
grantToken i (Token o q) = case srDequeue q of
  Just (q2,i2) | i == seGet o -> Right $ Token (seSet i2 o) q2
               | otherwise -> Left NotOwner
  Nothing -> Left NotRequested

data TokenG i c
  = TokenG (Token i)
  deriving (Show,Eq,Ord)

instance (Ord i) => Semigroup (TokenG i c) where
  TokenG t1 <> TokenG t2 = TokenG (t1 <> t2)

instance (Ord i, Cap c) => CoordSys (TokenG i c) where
  type GCap (TokenG i c) = c
  type GId (TokenG i c) = i
  resolveCaps i cs (TokenG t)
    | i == tokenOwner t = Right idE
    | otherwise = Left . Just . TokenG $ requestToken i t
  resolveEffect i e g@(TokenG t)
    | i == tokenOwner t = Right g
    | otherwise = Left . mincap $ e
  localCaps i (TokenG t)
    | i == tokenOwner t = fullCaps
    | otherwise = emptyCaps
  grantRequests i (TokenG t) = TokenG $ handleTokenReqs i t

{-| Grant a token if appropriate (@i@ is owner and token has been
  requested), or make no change otherwise. -}
handleTokenReqs :: (Eq i) => i -> Token i -> Token i
handleTokenReqs i t = case grantToken i t of
                        Right t' -> t'
                        Left _ -> t

data EscrowIntRequest i
  = EscrowIntRequest { _eprAsker :: i
                     , _eprAmount :: Int
                     }
  deriving (Show,Eq,Ord)

data EscrowIntAccount i
  = EscrowIntAccount { _epaOwned :: SECell Int
                     , _epaInbox :: SRQueue Int
                     , _epaRequests :: SRQueue (EscrowIntRequest i)
                     }
  deriving (Show,Eq,Ord)

makeLenses ''EscrowIntAccount

initAccount :: Int -> EscrowIntAccount i
initAccount n = EscrowIntAccount (seInit n) srEmpty srEmpty

instance (Ord i) => Semigroup (EscrowIntAccount i) where
  EscrowIntAccount o1 i1 r1 <> EscrowIntAccount o2 i2 r2 =
    EscrowIntAccount (o1 <> o2) (i1 <> i2) (r1 <> r2)

instance (Ord i) => Monoid (EscrowIntAccount i) where
  mempty = initAccount 0

data EscrowIntPool i
  = EscrowIntPool { _epAccounts :: Map i (EscrowIntAccount i)
                  , _epSinks :: [i]
                  , _epSources :: [i]
                  }
  deriving (Show,Eq,Ord)

makeLenses ''EscrowIntPool

acct :: (Ord i) => i -> Lens' (EscrowIntPool i) (EscrowIntAccount i)
acct i = epAccounts . at i . non mempty

instance (Ord i) => Semigroup (EscrowIntPool i) where
  EscrowIntPool a1 sk1 sr1 <> EscrowIntPool a2 _ _ =
    EscrowIntPool (Map.unionWith (<>) a1 a2) sk1 sr1

initEscrow :: [i] -> [i] -> Map i Int -> EscrowIntPool i
initEscrow sinks sources amounts =
  EscrowIntPool { _epSinks = sinks
                , _epSources = sources
                , _epAccounts = Map.map initAccount amounts
                }

escrowOwned :: (Ord i) => i -> EscrowIntPool i -> Int
escrowOwned i p = seGet $ p ^. epAccounts . at i . non mempty . epaOwned

escrowUnowned :: (Ord i) => i -> EscrowIntPool i -> Int
escrowUnowned i p = escrowTotal p - escrowOwned i p

escrowTotal :: (Ord i) => EscrowIntPool i -> Int
escrowTotal p = sum $ map (\i -> escrowOwned i p) (Map.keys (p^.epAccounts))

escrowUse
  :: (Ord i)
  => i
  -> Int
  -> EscrowIntPool i
  -> Either Int (EscrowIntPool i)
escrowUse i amt p | escrowOwned i p < amt = Left $ amt - escrowOwned i p
                  | otherwise = Right $
  p & acct i . epaOwned %~ seMod (\a -> a - amt)

escrowAdd
  :: (Ord i)
  => i
  -> Int
  -> EscrowIntPool i
  -> EscrowIntPool i
escrowAdd i amt = acct i . epaOwned %~ seMod (+ amt)

escrowRequest
  :: (Ord i)
  => i
  -> (i,Int)
  -> EscrowIntPool i
  -> EscrowIntPool i
escrowRequest i (i2,amt) =
  acct i2 . epaRequests
  %~ srEnqueue (EscrowIntRequest { _eprAsker = i
                                 , _eprAmount = amt })

escrowRequest'
  :: (Ord i)
  => i
  -> Int
  -> EscrowIntPool i
  -> Maybe (EscrowIntPool i)
escrowRequest' i amt p =
  case p ^. epSources of
    i2 : _ -> Just $ escrowRequest i (i2,amt) p
    [] -> Nothing

escrowTransfer
  :: (Ord i)
  => i
  -> (i,Int)
  -> EscrowIntPool i
  -> Either Int (EscrowIntPool i)
escrowTransfer i (i2,amt) p = do
  p' <- escrowUse i amt p
  return $ p' & acct i2 . epaInbox %~ srEnqueue amt

escrowHandleReqs :: (Ord i) => i -> EscrowIntPool i -> EscrowIntPool i
escrowHandleReqs i p =
  case srDequeue (p ^. acct i . epaRequests) of
    Just (rs',EscrowIntRequest i2 amt) ->
      case escrowTransfer i (i2,amt) p of
        Right p' -> escrowHandleReqs i (p' & acct i . epaRequests .~ rs')
        Left _ -> p
    Nothing -> p

escrowAccept :: (Ord i) => i -> EscrowIntPool i -> EscrowIntPool i
escrowAccept i p =
  let (inbox',amts) = srDequeueAll $ p ^. acct i . epaInbox
  in p & acct i . epaInbox .~ inbox'
       & acct i . epaOwned %~ seMod (+ sum amts)
