{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module UCap.Replica.MRep
  ( RId
  , MCS
  , BMsg
  , BMsg'
  , TBM
  , TBM'
  , MRepInfo (..)
  , evalMRepScript
  ) where

import Lang.Rwa
import Lang.Rwa.Interpret
import UCap.Coord
import UCap.Domain
import UCap.Lens
import UCap.Replica

import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans (liftIO)
import Data.Aeson hiding ((.=))
import GHC.Generics

type RId = String

class (GId g ~ RId, Eq g, Eq (GEffect g), CoordSys g) => MCS g
instance (GId g ~ RId, Eq g, Eq (GEffect g), CoordSys g) => MCS g

data BMsg g e
  = BPing VC
  | BPong VC
  | BNewEffect VC e g
  | BCoord VC g
  | BComplete (VThread String e) g
  | BRequest
  deriving (Show,Eq,Ord,Generic)

instance (ToJSON e, ToJSON g) => ToJSON (BMsg g e)
instance (FromJSON e, FromJSON g) => FromJSON (BMsg g e)

type BMsg' g = BMsg g (GEffect g)

type TBM g e = (RId, BMsg g e)

type TBM' g = TBM g (GEffect g)

type VC = VClock RId

data MRepState g e s
  = MRepState { _hrInitState :: s
              , _hrCurrentState :: s
              , _hrCoord :: g
              , _hrDag :: VThread RId e
              }

makeLenses ''MRepState

data MRepInfo g e
  = MRepInfo { _hrId :: RId
             , _hrOtherIds :: [RId]
             , _hrSend :: RId -> RId -> BMsg g e -> IO ()
             , _hrInbox :: TChan (TBM g e)
             }

makeLenses ''MRepInfo

type MRepT g = StateT (MRepState g (GEffect g) (GState g))
                      (ReaderT (MRepInfo g (GEffect g)) IO)

runMRep
  :: (MCS g)
  => MRepT g a
  -> MRepState g (GEffect g) (GState g)
  -> MRepInfo g (GEffect g)
  -> IO (a, MRepState g (GEffect g) (GState g))
runMRep m s r = runReaderT (runStateT m s) r

evalMRepScript
  :: (MCS g)
  => ScriptT g IO a
  -> GState g
  -> g
  -> MRepInfo g (GEffect g)
  -> IO a
evalMRepScript sc s0 g0 info =
  let st = MRepState { _hrInitState = s0
                     , _hrCurrentState = s0
                     , _hrCoord = g0
                     , _hrDag = initThreads
                     }
  in fst <$> runMRep (mrAwaitScript sc) st info


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
  rid <- view hrId
  g0 <- use hrCoord
  gUp <- case mg of
    Just g | g /= g0 -> do
      hrCoord %= (<> g)
      return True
    _ -> return False
  eUp <- logEffect e
  if gUp || eUp
     then mrCoord
     else return ()

logEffect :: (MCS g) => GEffect g -> MRepT g Bool
logEffect e | e == idE = return False
logEffect e = do
  rid <- view hrId
  v <- fst . getThread rid <$> use hrDag
  hrDag %= event rid e
  g <- use hrCoord
  mrBroadcast $ BNewEffect v e g
  return True

mrCoord :: (MCS g) => MRepT g ()
mrCoord = do
  rid <- view hrId
  v <- fst . getThread rid <$> use hrDag
  g <- use hrCoord
  mrBroadcast $ BCoord v g

mrUnicast :: (MCS g) => RId -> BMsg' g -> MRepT g ()
mrUnicast i msg = do
  rid <- view hrId
  send <- view hrSend
  liftIO $ send rid i msg

mrBroadcast :: (MCS g) => BMsg' g -> MRepT g ()
mrBroadcast msg = mapM_ (\i -> mrUnicast i msg) =<< view hrOtherIds

handleMsg :: (MCS g) => TBM' g -> MRepT g Bool
handleMsg (i,msg) = do
  rid <- view hrId
  case msg of
    BNewEffect v e g -> do
      hrDag `eitherModifying` eventImport rid (v,e) >>= \case
        Right () -> return True
        Left NotCausal -> mrRequestComplete i >> return False
        Left IncompleteClock -> error $ "Failed to import from" ++ show i
    BPing v -> do
      b <- hrDag `maybeModifying` updateClock i v
      v1 <- getClock rid <$> use hrDag
      mrUnicast i $ BPong v1
      return b
    BPong v -> do
      hrDag `maybeModifying` updateClock i v
    BCoord v g -> hrCoord <>= g >> return True
    BComplete dag1 g -> 
      hrDag `eitherModifying` mergeThread dag1 >>= \case
        Right () -> return True
        Left i2 -> error $ "BComplete error, cannot merge on " 
                           ++ show i2
    BRequest -> do
      dag <- use hrDag
      g <- use hrCoord
      mrUnicast i $ BComplete dag g
      return False

mrRequestComplete :: (MCS g) => RId -> MRepT g ()
mrRequestComplete i = do
  rid <- view hrId
  mrUnicast i BRequest

mrAwaitScript :: (MCS g) => ScriptT g IO a -> MRepT g a
mrAwaitScript sc = mrScript sc >>= \case
  Right a -> return a
  Left b -> mrTryAwait b >>= \case
    Just sc' -> mrCheckChange >> mrAwaitScript sc'
    Nothing -> mrWaitChange >> mrAwaitScript (await b)

mrCheckChange :: (MCS g) => MRepT g ()
mrCheckChange = do
  chan <- view hrInbox
  m <- liftIO . atomically $ tryReadTChan chan
  case m of
    Just msg -> handleMsg msg >> return ()
    Nothing -> return ()

mrWaitChange :: (MCS g) => MRepT g ()
mrWaitChange = do
  chan <- view hrInbox
  msg <- liftIO . atomically $ readTChan chan
  r <- handleMsg msg
  case r of
    True -> return ()
    False -> mrWaitChange
