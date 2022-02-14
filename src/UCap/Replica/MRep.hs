{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module UCap.Replica.MRep where

import Lang.Rwa
import Lang.Rwa.Interpret
import UCap.Coord
import UCap.Domain
import UCap.Lens
import UCap.Replica

import Control.Concurrent.STM.TChan
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans (liftIO)
import Data.Aeson hiding ((.=))
import GHC.Generics

class ( GId g ~ String, Eq g, Eq (GEffect g), CoordSys g) => MCS g

data ESeq e
  = ESeq { eseqPayload :: [(VC, e)]
         }
  deriving (Show,Eq,Ord,Generic)

instance (ToJSON e) => ToJSON (ESeq e)
instance (FromJSON e) => FromJSON (ESeq e)

data BMsg g e
  = Hello VC
  | BCast (ESeq e) g
  | BComplete (VThread String e) g
  | ERequest Int
  deriving (Show,Eq,Ord,Generic)

instance (ToJSON e, ToJSON g) => ToJSON (BMsg g e)
instance (FromJSON e, FromJSON g) => FromJSON (BMsg g e)

type BMsg' g = BMsg g (GEffect g)

type TBM g e = (String, BMsg g e)

type VC = VClock String

data MRepState g e s
  = MRepState { _hrInitState :: s
              , _hrCurrentState :: s
              , _hrCoord :: g
              , _hrDag :: VThread String e
              , _hrSendQueue :: [(VC,e)]
              , _hrInbox :: TChan (TBM g e)
              }

makeLenses ''MRepState

data MRepInfo g e
  = MRep { _hrId :: String
         , _hrOtherIds :: [String]
         , _hrSend :: String -> String -> BMsg g e -> IO ()
         }

makeLenses ''MRepInfo

type MRepT g = StateT (MRepState g (GEffect g) (GState g))
                      (ReaderT (MRepInfo g (GEffect g)) IO)

mrScript :: (MCS g) => ScriptT g IO a -> MRepT g (Either (ScriptB g IO a) a)
mrScript sc = do
  rid <- view hrId
  liftIO (unwrapScript sc rid) >>= \case
    Left (ReadState f) -> mrScript . f =<< mrReadState
    Left (WriteState ctx sc') -> mrWriteState ctx >> mrScript sc'
    Left (Await acs) -> mrTryAwait acs >>= \case
      Just sc' -> mrScript sc'
      Nothing -> return (Left acs)
    Right a -> return (Right a)

mrTryAwait :: (MCS g) => ScriptB g IO a -> MRepT g (Maybe (ScriptT g IO a))
mrTryAwait b = do
  rid <- view hrId
  state <- mrReadState
  liftIO $ runReaderT (checkBlock b state) rid

mrReadState :: (MCS g) => MRepT g (ReadRep (RepCtx' g))
mrReadState = RepCtx <$> use hrCurrentState <*> use hrCoord

mrWriteState :: (MCS g) => RepCtx' g -> MRepT g ()
mrWriteState (RepCtx e mg) = do
  g0 <- use hrCoord
  gUp <- case mg of
    Just g | g /= g0 -> do
      hrCoord %= (<> g)
      return True
    _ -> return False
  eUp <- logEffect e
  if gUp || eUp
     then mrBroadcast
     else return ()

logEffect :: (MCS g) => GEffect g -> MRepT g Bool
logEffect e | e == idE = return False
logEffect e = do
  rid <- view hrId
  v <- fst . getThread rid <$> use hrDag
  hrDag %= event rid e
  hrSendQueue %= (++ [(v,e)])
  return $ True

mrBroadcast :: (MCS g) => MRepT g ()
mrBroadcast = do
  rid <- view hrId
  others <- view hrOtherIds
  g <- use hrCoord
  efq <- use hrSendQueue
  hrSendQueue .= []
  send <- view hrSend
  let msg = BCast (ESeq efq) g
  liftIO $ mapM_ (\i -> send rid i msg) others
