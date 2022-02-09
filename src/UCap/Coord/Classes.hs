{-# LANGUAGE FlexibleContexts #-}
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
    the coordination system.  If so, return @'Right' ()@.  If not,
    attempt to perform requests that will eventually satisfy those
    requirements.  If this is possible, return the request-containing
    system in @'Left' ('Just' y)@.  If not, and thus there is no way
    to satisfy the requirements, return @'Left' 'Nothing'@. -}
  resolveCaps :: GId g -> Caps (GCap g) -> g -> Either (Maybe g) ()
  {-| For given effect, modify the coordination system to reflect its use.
    If the coordination system locally permits issuing the effect,
    then modify it accordingly and return @'Right' y@.  If not, return
    @'Left' c@, indicating a write failure with excess write
    capability @c@. -}
  resolveEffect :: GId g -> GEffect g -> g -> Either (GCap g) g

  {-| The locally-held read and write capabilities. -}
  localCaps :: GId g -> g -> Caps (GCap g)

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

initAccount :: Int -> EscrowIntAccount i
initAccount n = EscrowIntAccount (seInit n) srEmpty srEmpty

instance (Ord i) => Semigroup (EscrowIntAccount i) where
  EscrowIntAccount o1 i1 r1 <> EscrowIntAccount o2 i2 r2 =
    EscrowIntAccount (o1 <> o2) (i1 <> i2) (r1 <> r2)

data EscrowIntPool i
  = EscrowPool { _epAccounts :: Map i (EscrowIntAccount i)
               , _epSinks :: [i]
               , _epSource :: [i]
               }
  deriving (Show,Eq,Ord)

instance (Ord i) => Semigroup (EscrowIntPool i) where
  EscrowPool a1 sk1 sr1 <> EscrowPool a2 _ _
    = EscrowPool (Map.unionWith (<>) a1 a2) sk1 sr1
