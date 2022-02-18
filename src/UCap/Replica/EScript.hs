{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module UCap.Replica.EScript
  ( EScriptT
  , EScriptTerm
  , EScriptB
  , unwrapEScript
  , trScript
  , trBlock
  , ExWr (..)
  , ExRd (..)
  , TermStatus
  , module Lang.Rwa
  , module Lang.Rwa.Interpret
  ) where

import Lang.Rwa
import Lang.Rwa.Interpret
import UCap.Coord
import UCap.Domain.Classes
import UCap.Lens
import UCap.Op
import UCap.Replica.Script
import UCap.Replica.Transact

import Control.Monad.Except
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map

data TermStatus
  = TermComplete
  | TermAbort
  deriving (Show,Eq,Ord)

{-| Write-type for experiment scripts.

  'exwStore' accesses the store update, which is a payload effect and
  ('Maybe') a new coordination system value.

  'exwTerminated' lists IDs of transactions that have either been
  completed or have aborted.

  'exwReceived' lists IDs of transaction that have been popped off the
  queue and are now in progress.  This should be removed from the
  transaction queue that is presented to the script in the next read.
-}
data ExWr g e
  = ExWr { _exwStore :: RepCtx (Maybe g) e
         , _exwTerminated :: [(Int,TermStatus)]
         , _exwReceived :: [Int]
         }
  deriving (Show,Eq,Ord)

makeLenses ''ExWr

{-| Read-type for experiment scripts.

  'exrStore' is the transaction store state (payload state and
  coordination system state).

  'exrQueue' is a map of transactions that have been requested.
-}
data ExRd g s
  = ExRd { _exrStore :: RepCtx g s
         , _exrQueue :: Map Int (Op g (ExceptT () IO) () ())
         , _exrShutdown :: Bool
         }

makeLenses ''ExRd

instance (Semigroup g, Semigroup e) => Semigroup (ExWr g e) where
  ExWr s1 t1 r1 <> ExWr s2 t2 r2 =
    ExWr (s1 <> s2) (t1 ++ t2) (r1 ++ r2)

instance (Monoid g, Monoid e) => Monoid (ExWr g e) where
  mempty = ExWr mempty [] []

instance (EffectDom e) => RwState (ExWr g e) where
  type ReadRep (ExWr g e) = ExRd g (EDState e)

{-| Script that embeds a transaction store script, and additionally
  interacts with experiments. -}
type EScriptT g a = Rwa (ExWr g (GEffect g)) (ReaderT (GId g) IO) a

type EScriptTerm g a = RwaTerm (ExWr g (GEffect g)) (ReaderT (GId g) IO) a

type EScriptB g a =
       Block (ExWr g (GEffect g)) (ReaderT (GId g) IO) (EScriptT g a)

{-| Interpret a transaction store script as an experiment script. -}
trScript :: ScriptT g IO a -> EScriptT g a
trScript sc = do
  rid <- ask
  liftIO (unwrapScript sc rid) >>= \case
    Left (ReadState f) -> wrap . ReadState $
      trScript . f . view exrStore
    Left (WriteState ctx sc') -> 
      wrap $ WriteState (ExWr ctx [] []) (trScript sc')
    Left (Await b) -> wrap . Await $ trBlock b
    -- Left (Await (Block m)) -> wrap . Await . Block $
    --   do rdv <- ask
    --      rid <- lift . lift $ ask
    --      r <- liftIO $ runReaderT (runReaderT (runExceptT m)
    --                                           (rdv^.exrStore))
    --                               rid
    --      case r of
    --        Right sc' -> return $ trScript sc'
    --        Left e -> throwError e
    Right a -> return a

trBlock :: ScriptB g IO a -> EScriptB g a
trBlock (Block m) = Block $
  do rdv <- ask
     rid <- lift . lift $ ask
     r <- liftIO $ runReaderT (runReaderT (runExceptT m)
                                          (rdv^.exrStore))
                              rid
     case r of
       Right sc' -> return $ trScript sc'
       Left e -> throwError e

unwrapEScript
  :: EScriptT g a
  -> GId g
  -> IO (Either (EScriptTerm g (EScriptT g a)) a)
unwrapEScript sc i = runReaderT (nextTerm sc) i

{-| Await a 'Block', or skip it and terminate if the 'exrShutdown' flag
  in the state is 'True'. -}
awaitSD :: EScriptB g () -> EScriptT g ()
awaitSD b = await . firstOf $
  [ (do s <- checkState
        (s^.exrShutdown) ?> return ())
  , b
  ]

{-| Loop, but shutdown when requested. -}
loopSD :: EScriptB g a -> EScriptT g ()
loopSD b = awaitSD (b `andThen_` return ()) >> loopSD b

{-| Run a sequence of transactions, but shutdown when requested. -}
transactManySD_
  :: (CoordSys g)
  => [Op (GCap g) IO () ()]
  -> EScriptT g ()
transactManySD_ [] = return ()
transactManySD_ (o1:os) = do
  complete1 <- trScript $ transact o1
  awaitSD . firstOf $
    [ trBlock grantRequests' `andThen_` transactManySD_ (o1:os)
    , trBlock acceptGrants' `andThen_` transactManySD_ (o1:os)
    , trBlock complete1 `andThen_` transactManySD_ os
    ]
