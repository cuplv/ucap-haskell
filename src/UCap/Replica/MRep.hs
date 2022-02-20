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
  , BMsg (..)
  , BMsg'
  , TBM
  , TBM'
  , Addrs (..)
  , MRepInfo (..)
  , evalMRepScript
  , evalMRepScript'
  ) where

import Lang.Rwa
import Lang.Rwa.Interpret
import UCap.Coord
import UCap.Domain
import UCap.Lens
import UCap.Replica
import UCap.Replica.EScript

import Control.Concurrent.STM
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans (liftIO)
import Data.Aeson hiding ((.=))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import GHC.Generics

type RId = String

class (GId g ~ RId, Eq g, Eq (GEffect g), CoordSys g, Show g) => MCS g
instance (GId g ~ RId, Eq g, Eq (GEffect g), CoordSys g, Show g) => MCS g

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
              , _hrMsgWaiting :: [TBM g e]
              , _hrShutdownActive :: Bool
              }

makeLenses ''MRepState

type Addrs = Map RId (String,Int)

data MRepInfo g e
  = MRepInfo { _hrId :: RId
             , _hrAddrs :: Addrs
             , _hrSend :: RId -> RId -> BMsg g e -> IO ()
             , _hrInbox :: TChan (TBM g e)
             , _hrDebug :: String -> IO ()
             , _hrShutdown :: TMVar ()
             }

makeLenses ''MRepInfo

type MRepT g = 
       ExceptT () 
         (StateT (MRepState g (GEffect g) (GState g))
            (ReaderT (MRepInfo g (GEffect g)) IO))

mrDebug :: String -> MRepT g ()
mrDebug s = do
  f <- view hrDebug
  liftIO $ f s

mrOtherIds :: MRepT g [RId]
mrOtherIds = do
  rid <- view hrId
  ids <- Map.keys <$> view hrAddrs
  return $ List.delete rid ids

runMRep
  :: (MCS g)
  => MRepT g a
  -> MRepState g (GEffect g) (GState g)
  -> MRepInfo g (GEffect g)
  -> IO (Either () a, MRepState g (GEffect g) (GState g))
runMRep m s r = runReaderT (runStateT (runExceptT m) s) r

evalMRepScript
  :: (MCS g)
  => EScriptT g a
  -> GState g
  -> g
  -> MRepInfo g (GEffect g)
  -> IO (Either () a)
evalMRepScript sc s0 g0 info =
  fst <$> evalMRepScript' sc s0 g0 info

evalMRepScript'
  :: (MCS g)
  => EScriptT g a
  -> GState g
  -> g
  -> MRepInfo g (GEffect g)
  -> IO (Either () a,GState g)
evalMRepScript' sc s0 g0 info =
  let st = MRepState { _hrInitState = s0
                     , _hrCurrentState = s0
                     , _hrCoord = g0
                     , _hrDag = initThreads
                     , _hrMsgWaiting = []
                     , _hrShutdownActive = False
                     }
      m = do mrPing
             -- In case effects have been missed due to slow start-up,
             -- we request a complete copy of the state from all
             -- listening peers.
             mrRequestAll
             mrAwaitScript sc
  in do (a,m) <- runMRep m st info
        return (a, m ^. hrCurrentState)


mrAllIds :: (MCS g) => MRepT g [RId]
mrAllIds = Map.keys <$> view hrAddrs

mrPrune :: (MCS g) => MRepT g ()
mrPrune = do
  ids <- mrAllIds
  t <- use hrDag
  let (td, t') = prune ids t
  hrDag .= t'
  let ed = mconcat . serialize $ td
  hrInitState %= eFun ed

mrScript
  :: (MCS g)
  => EScriptT g a
  -> MRepT g (Either (EScriptB g a) a)
mrScript sc = do
  rid <- view hrId
  liftIO (unwrapEScript sc rid) >>= \case
    Left (ReadState f) -> mrScript . f =<< mrReadState
    Left (WriteState ctx sc') -> mrWriteState ctx >> mrScript sc'
    Left (Await acs) -> mrTryAwait acs >>= \case
      Just sc' -> mrScript sc'
      Nothing -> return (Left acs)
    Right a -> return (Right a)

mrTryAwait :: (MCS g) => EScriptB g a -> MRepT g (Maybe (EScriptT g a))
mrTryAwait b = do
  rid <- view hrId
  state <- mrReadState
  liftIO $ runReaderT (checkBlock b state) rid

mrReadState :: (MCS g) => MRepT g (ExRd g (GState g))
mrReadState = do
  ctx <- RepCtx <$> use hrCurrentState <*> use hrCoord
  sd <- use hrShutdownActive
  return $ ExRd
    { _exrStore = ctx
    , _exrQueue = Map.empty
    , _exrShutdown = sd
    }

mrWriteState :: (MCS g) => ExWr g (GEffect g) -> MRepT g ()
mrWriteState (ExWr (RepCtx e mg) q sd) = do
  rid <- view hrId
  g0 <- use hrCoord
  gUp <- case mg of
    Just g | g0 <> g /= g0 -> do
      hrCoord %= (<> g)
      return True
    _ -> return False
  eUp <- logEffect e
  if gUp && not eUp
     then mrCoord
     else return ()

updateStateVal :: (MCS g) => MRepT g ()
updateStateVal = do
  s0 <- use hrInitState
  dag <- use hrDag
  let s' = eFun (mconcat . serialize $ dag) s0
  hrCurrentState .= s'

logEffect :: (MCS g) => GEffect g -> MRepT g Bool
logEffect e | e == idE = return False
logEffect e = do
  rid <- view hrId
  vz <- fst . getThread rid <$> use hrDag
  v <- mrGetClock
  if v /= vz
     then error $ "Clocks dont' match: " ++ show v ++ " vs. " ++ show vz
     else return ()
  hrDag %= event rid e
  mrPrune
  updateStateVal
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
mrBroadcast msg = mapM_ (\i -> mrUnicast i msg) =<< mrOtherIds

mrPing :: (MCS g) => MRepT g ()
mrPing = do
  rid <- view hrId
  v <- mrGetClock
  mrBroadcast $ BPing v

mrGetClock :: (MCS g) => MRepT g VC
mrGetClock = totalClock <$> use hrDag

hrClearWaiting :: (MCS g) => MRepT g ()
hrClearWaiting = do
  ms <- use hrMsgWaiting
  let f ms msg = handleMsg msg >>= \case
                   MsgNonCausal -> return $ ms ++ [msg]
                   _ -> return ms
  ms' <- foldM f [] ms
  hrMsgWaiting .= ms'

data MsgResult
  = MsgUpdate -- ^ A change has been made
  | MsgOld -- ^ No new information
  | MsgNonCausal -- ^ Could not handle yet

handleMsg :: (MCS g) => TBM' g -> MRepT g MsgResult
handleMsg (i,msg) = do
  rid <- view hrId
  case msg of
    BNewEffect v e g -> do
      mrDebug $ "Handle BNewEffect from " ++ show i
      let tryImport d = observe rid i <$> eventImport' i (v,e) d
      hrDag `eitherModifying` tryImport >>= \case
        Right () -> do updateStateVal
                       v' <- mrGetClock
                       mrDebug $ "Clock up to " ++ show v'
                       return MsgUpdate
        Left NotCausal -> return MsgNonCausal
        Left IncompleteClock -> error $ "Failed to import from" ++ show i
    BPing v -> do
      let pong = do v1 <- mrGetClock
                    mrUnicast i $ BPong v1
      hrDag `eitherModifying` updateClock i v >>= \case
        Left MissingEvents -> return MsgNonCausal
        Left OldValues -> pong >> return MsgOld
        Right () -> pong >> return MsgUpdate
    BPong v -> do
      hrDag `eitherModifying` updateClock i v >>= \case
        Left MissingEvents -> return MsgNonCausal
        Left OldValues -> return MsgOld
        Right () -> return MsgUpdate
    BCoord v g -> do
      mrDebug $ "Handle BCoord from " ++ show i ++ ", " ++ show g
      hrDag `eitherModifying` updateClock i v >>= \case
        Left MissingEvents -> return MsgNonCausal
        _ -> do hrCoord <>= g
                g' <- use hrCoord
                mrDebug $ "Updated to " ++ show g'
                return MsgUpdate
    BComplete dag1 g -> do
      mrDebug $ "Handle BComplete from " ++ show i
      hrDag `eitherModifying` mergeThread dag1 >>= \case
        Right () -> updateStateVal >> return MsgUpdate
        Left i2 -> error $ "BComplete error, cannot merge on " 
                           ++ show i2
    BRequest -> do
      mrDebug $ "Handle BRequest from " ++ show i
      dag <- use hrDag
      g <- use hrCoord
      mrUnicast i $ BComplete dag g
      return MsgOld

mrRequestAll :: (MCS g) => MRepT g ()
mrRequestAll = do
  rid <- view hrId
  mrBroadcast BRequest

mrAwaitScript :: (MCS g) => EScriptT g a -> MRepT g a
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
    Just msg -> handleMsg msg >> mrPrune
    Nothing -> return ()

data MrChange g e
  = MrGotMsg (TBM g e)
  | MrShutdown
  deriving (Show,Eq,Ord)

mrWaitChange :: (MCS g) => MRepT g ()
mrWaitChange = do
  chan <- view hrInbox
  sd <- view hrShutdown
  let stm = 
        (MrGotMsg <$> readTChan chan)
        `orElse` (MrShutdown <$ takeTMVar sd)
  cmd <- liftIO . atomically $ stm
  case cmd of
    MrGotMsg msg -> handleMsg msg >>= \case
      MsgUpdate -> mrPrune
      MsgOld -> mrWaitChange
      MsgNonCausal -> do
        hrMsgWaiting %= (++ [msg])
        w <- use hrMsgWaiting
        mrDebug $ "Msgs waiting: " ++ show (length w)
        mrWaitChange
    MrShutdown -> do
      hrShutdownActive .= True
