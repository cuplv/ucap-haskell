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
  , Op'
  , ExprData
  ) where

import Lang.Rwa
import Lang.Rwa.Interpret
import UCap.Coord
import UCap.Domain
import UCap.Lens
import UCap.Op
import UCap.Replica
import UCap.Replica.Debug
import UCap.Replica.EScript

import Control.Concurrent.STM
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans (liftIO)
import Data.Aeson hiding ((.=))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock
import GHC.Generics

type RId = String

data PeerStatus
  = PeerStatus (Map RId Bool)
  deriving (Show,Eq,Ord,Generic)

instance ToJSON PeerStatus
instance FromJSON PeerStatus

instance Semigroup PeerStatus where
  PeerStatus m1 <> PeerStatus m2 = PeerStatus $ Map.unionWith (&&) m1 m2

allReady :: [RId] -> PeerStatus -> Bool
allReady is (PeerStatus m) = and $ map (`Map.member` m) is

allFinished :: [RId] -> PeerStatus -> Bool
allFinished is (PeerStatus m) = and $
  map (\k -> Map.lookup k m == Just False) is

initReady :: RId -> PeerStatus
initReady i = PeerStatus $ Map.singleton i True

setReady :: RId -> PeerStatus -> PeerStatus
setReady i (PeerStatus m) = PeerStatus $ Map.insert i True m

setFinished :: RId -> PeerStatus -> PeerStatus
setFinished i (PeerStatus m) = PeerStatus $ Map.insert i False m

class (GId g ~ RId, Eq g, Eq (GEffect g), CoordSys g, Show g) => MCS g
instance (GId g ~ RId, Eq g, Eq (GEffect g), CoordSys g, Show g) => MCS g

data BMsg g e
  = BPing PeerStatus VC
  | BPong PeerStatus VC
  | BNewEffect VC e g
  | BCoord VC g
  | BComplete (VThread String e) g
  | BRequest
  deriving (Show,Eq,Ord,Generic)

instance (ToJSON e, ToJSON g) => ToJSON (BMsg g e)
instance (FromJSON e, FromJSON g) => FromJSON (BMsg g e)

bmsgClock :: (MCS g) => BMsg g e -> VC
bmsgClock = \case
  BPing _ v -> v
  BPong _ v -> v
  BNewEffect v _ _ -> v
  BCoord v _ -> v
  BComplete _ _ -> zeroClock
  BRequest -> zeroClock

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
              , _hrQueue :: Map Int (Op' g)
              , _lastSentClock :: VC
              , _hrLiveNodes :: PeerStatus
              , _hrTrFinish :: Map Int UTCTime
              }

type Op' g = Op (GCap g) IO () ()

makeLenses ''MRepState

type Addrs = Map RId (String,Int)


data MRepInfo g e
  = MRepInfo { _hrId :: RId
             , _hrAddrs :: Addrs
             , _hrSend :: RId -> BMsg g e -> IO ()
             , _hrEOM :: IO ()
             , _hrInbox :: TChan (TBM g e)
             , _hrDebug :: Debug
             , _hrShutdown :: TMVar ()
             , _hrGetQueue :: TQueue (Int, Op' g)
             , _hrTrStart :: TVar (Map Int UTCTime)
             , _hrAllReady :: TMVar ()
             }

makeLenses ''MRepInfo

type MRepT g = 
       ExceptT () 
         (StateT (MRepState g (GEffect g) (GState g))
            (ReaderT (MRepInfo g (GEffect g)) IO))

mrDebug :: Int -> String -> MRepT g ()
mrDebug n s = do
  f <- view hrDebug
  liftIO $ f DbMainLoop n s

mrOtherIds :: MRepT g [RId]
mrOtherIds = do
  rid <- view hrId
  ids <- Map.keys <$> view hrAddrs
  return $ List.delete rid ids

mrAllReady :: MRepT g Bool
mrAllReady = do
  ids <- Map.keys <$> view hrAddrs
  pstat <- use hrLiveNodes
  return $ allReady ids pstat

mrAllFinished :: MRepT g Bool
mrAllFinished = do
  ids <- Map.keys <$> view hrAddrs
  pstat <- use hrLiveNodes
  return $ allFinished ids pstat

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
  -> IO (Either () (a, ExprData))
evalMRepScript sc s0 g0 info =
  fst <$> evalMRepScript' sc s0 g0 info

type ExprData = (Map Int UTCTime, Map Int UTCTime)

evalMRepScript'
  :: (MCS g)
  => EScriptT g a
  -> GState g
  -> g
  -> MRepInfo g (GEffect g)
  -> IO (Either () (a, ExprData),(GState g, g))
evalMRepScript' sc s0 g0 info =
  let st = MRepState { _hrInitState = s0
                     , _hrCurrentState = s0
                     , _hrCoord = g0
                     , _hrDag = initThreads
                     , _hrMsgWaiting = []
                     , _hrQueue = Map.empty
                     , _lastSentClock = zeroClock
                     , _hrLiveNodes = PeerStatus Map.empty
                     , _hrTrFinish = Map.empty
                     }
      m = do mrPing
             -- In case effects have been missed due to slow start-up,
             -- we request a complete copy of the state from all
             -- listening peers.
             mrUpdateLive $ initReady (info^.hrId)
             mrRequestAll
             a <- mrAwaitScript sc
             d <- mrCollectData

             -- Put an "end of messages" notification in senders'
             -- outboxes so that they do not wait for further messages
             -- after they finish their queues.
             eom <- view hrEOM
             liftIO eom

             return (a,d)
  in do (a,m) <- runMRep m st info
        return (a, (m ^. hrCurrentState, m ^. hrCoord))


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
  -> MRepT g (Either (Block' (EScriptT g) a) a)
mrScript sc = do
  rid <- view hrId
  liftIO (unwrapEScript sc rid) >>= \case
    Left (ReadState f) -> mrScript . f =<< mrReadState
    Left (WriteState ctx sc') -> mrWriteState ctx >> mrScript sc'
    Left (Await acs) -> mrTryAwait acs >>= \case
      Just sc' -> mrScript sc'
      Nothing -> return (Left acs)
    Right a -> return (Right a)

mrTryAwait
  :: (MCS g)
  => Block' (EScriptT g) a
  -> MRepT g (Maybe (EScriptT g a))
mrTryAwait b = do
  rid <- view hrId
  state <- mrReadState
  liftIO $ runReaderT (checkBlock b state) rid

mrReadState :: (MCS g) => MRepT g (ExRd g (GState g))
mrReadState = do
  ctx <- RepCtx <$> use hrCurrentState <*> use hrCoord
  q <- use hrQueue
  finished <- mrAllFinished
  mrDebug 3 $ "Passing along " ++ show (length q) ++ " trs."
  return $ ExRd
    { _exrStore = ctx
    , _exrQueue = q
    , _exrShutdown = finished
    }

mrWriteState :: (MCS g) => ExWr g (GEffect g) -> MRepT g ()
mrWriteState (ExWr (RepCtx e mg) trm rc) = do
  mapM_ (\i -> hrQueue %= Map.delete i) rc
  mrDebug 3 $ show rc
  if not $ null trm
     then do t <- liftIO getCurrentTime
             let f (i,TermComplete) = hrTrFinish %= Map.insert i t
             mapM_ f trm
             mrDebug 3 $ show trm
     else return ()

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
     then error $ "Clocks don't match: " ++ show v ++ " vs. " ++ show vz
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
  send <- view hrSend
  liftIO $ send i msg

mrBroadcast :: (MCS g) => BMsg' g -> MRepT g ()
mrBroadcast msg = do
  mapM_ (\i -> mrUnicast i msg) =<< mrOtherIds
  lastSentClock %= joinVC (bmsgClock msg)

mrPing :: (MCS g) => MRepT g ()
mrPing = do
  rid <- view hrId
  v <- mrGetClock
  lv <- use hrLiveNodes
  mrBroadcast $ BPing lv v

mkPing :: (MCS g) => MRepT g (BMsg' g)
mkPing = do
  v <- mrGetClock
  lv <- use hrLiveNodes
  return $ BPing lv v

mkPong :: (MCS g) => MRepT g (BMsg' g)
mkPong = do
  v <- mrGetClock
  lv <- use hrLiveNodes
  return $ BPong lv v

mrGetClock :: (MCS g) => MRepT g VC
mrGetClock = totalClock <$> use hrDag

mrClearWaiting :: (MCS g) => MRepT g ()
mrClearWaiting = do
  ms <- use hrMsgWaiting
  let f ms msg = handleMsg msg >>= \case
                   MsgNonCausal -> return $ ms ++ [msg]
                   _ -> return ms
  ms' <- foldM f [] ms
  hrMsgWaiting .= ms'
  if length ms' /= length ms
     then mrClearWaiting
     else return ()

data MsgResult
  = MsgUpdate -- ^ A change has been made
  | MsgOld -- ^ No new information
  | MsgNonCausal -- ^ Could not handle yet

handleMsg :: (MCS g) => TBM' g -> MRepT g MsgResult
handleMsg (i,msg) = do
  rid <- view hrId
  mrDebug 2 $ "Handling message..."
  case msg of
    BNewEffect v e g -> do
      mrDebug 1 $ "Handle BNewEffect from " ++ show i ++ " " ++ show v
      let tryImport d = observe rid i <$> eventImport' i (v,e) d
      hrDag `eitherModifying` tryImport >>= \case
        Right () -> do updateStateVal
                       v' <- mrGetClock
                       mrDebug 1 $ "Clock up to " ++ show v'
                       mrCompareClock
                       return MsgUpdate
        Left NotCausal -> return MsgNonCausal
        Left IncompleteClock -> error $ "Failed to import from" ++ show i
    BPing lv v -> do
      mrUpdateLive lv
      let pong = mrUnicast i =<< mkPong
      hrDag `eitherModifying` updateClock i v >>= \case
        Left MissingEvents -> return MsgNonCausal
        Left OldValues -> pong >> return MsgOld
        Right () -> pong >> return MsgUpdate
    BPong lv v -> do
      mrUpdateLive lv
      finished <- mrAllFinished
      hrDag `eitherModifying` updateClock i v >>= \case
        Left MissingEvents -> return MsgNonCausal
        Left OldValues | finished -> return MsgUpdate
                       | otherwise -> return MsgOld
        Right () -> return MsgUpdate
    BCoord v g -> do
      mrDebug 2 $ "Handle BCoord from " ++ show i ++ ", " ++ show g
      hrDag `eitherModifying` updateClock i v >>= \case
        Left MissingEvents -> return MsgNonCausal
        _ -> do hrCoord <>= g
                g' <- use hrCoord
                mrDebug 2 $ "Updated to " ++ show g'
                return MsgUpdate
    BComplete dag1 g -> do
      mrDebug 2 $ "Handle BComplete from " ++ show i
      hrDag `eitherModifying` mergeThread dag1 >>= \case
        Right () -> do
          v' <- mrGetClock
          hrDag `eitherModifying` updateClock rid v' >>= \case
            Right () -> updateStateVal >> return MsgUpdate
            Left MissingEvents -> error "Missing events after BComplete"
            Left OldValues -> return MsgOld
        Left i2 -> error $ "BComplete error, cannot merge on " 
                           ++ show i2
    BRequest -> do
      mrDebug 2 $ "Handle BRequest from " ++ show i
      dag <- use hrDag
      g <- use hrCoord
      mrUnicast i $ BComplete dag g
      return MsgOld

mrUpdateLive :: (MCS g) => PeerStatus -> MRepT g ()
mrUpdateLive lv = do
  lv' <- hrLiveNodes <%= (<> lv)
  mrDebug 1 $ "Active nodes: " ++ show lv'
  ready <- mrAllReady
  if ready
     then do a <- view hrAllReady
             liftIO.atomically $ tryPutTMVar a ()
             return ()
     else return ()

mrCompareClock :: (MCS g) => MRepT g ()
mrCompareClock = do
  v1 <- mrGetClock
  v2 <- use lastSentClock
  dag <- use hrDag
  mrDebug 2 $ "Clock diff at " ++ show (aggDiff v1 v2)
  mrDebug 2 $ "Dag size at " ++ show (sizeVT dag)
  if aggDiff v1 v2 >= 10
     then mrBroadcast =<< mkPong
     else return ()

mrRequestAll :: (MCS g) => MRepT g ()
mrRequestAll = do
  rid <- view hrId
  mrBroadcast BRequest

mrCollectData :: (MCS g) => MRepT g ExprData
mrCollectData = do
  ss <- liftIO . readTVarIO =<< view hrTrStart
  fs <- use hrTrFinish
  return (ss,fs)

mrAwaitScript :: (MCS g) => EScriptT g a -> MRepT g a
mrAwaitScript sc = mrScript sc >>= \case
  Right a -> do
    return a
  Left b -> mrTryAwait b >>= \case
    Just sc' -> mrCheckChange >> mrAwaitScript sc'
    Nothing -> mrWaitChange >> mrAwaitScript (await b)

mrCheckChange :: (MCS g) => MRepT g ()
mrCheckChange = do
  chan <- view hrInbox
  m <- liftIO . atomically $ tryReadTChan chan
  case m of
    Just msg -> handleMsg msg >>= \case
      MsgUpdate -> mrClearWaiting >> mrPrune
      MsgOld -> return ()
      MsgNonCausal -> do
        hrMsgWaiting %= (++ [msg])
        w <- use hrMsgWaiting
        mrDebug 1 $ "Msgs waiting: " ++ show (length w)
        return ()
    Nothing -> return ()

data MrChange g e
  = MrGotMsg (TBM g e)
  | MrShutdown
  | MrNewTransact [(Int, Op' g)]

{- | Like 'Control.Concurrent.STM.TQueue.flushTQueue', but it retries
   when the queue is empty rather than returning @[]@. -}
flushTQueue1 :: TQueue a -> STM [a]
flushTQueue1 q = do
  b <- isEmptyTQueue q
  if not b
     then flushTQueue q
     else retry

mrWaitChange :: (MCS g) => MRepT g ()
mrWaitChange = do
  rid <- view hrId
  chan <- view hrInbox
  sd <- view hrShutdown
  tq <- view hrGetQueue
  let stm = 
        (MrGotMsg <$> readTChan chan)
        `orElse` (MrShutdown <$ takeTMVar sd)
        `orElse` (MrNewTransact <$> flushTQueue1 tq)
  cmd <- liftIO . atomically $ stm
  case cmd of
    MrGotMsg msg -> handleMsg msg >>= \case
      MsgUpdate -> mrClearWaiting >> mrPrune
      MsgOld -> mrWaitChange
      MsgNonCausal -> do
        hrMsgWaiting %= (++ [msg])
        w <- use hrMsgWaiting
        mrDebug 1 $ "Msgs waiting: " ++ show (length w)
        mrWaitChange
    MrShutdown -> do
      hrLiveNodes %= setFinished rid
      mrBroadcast =<< mkPong
    MrNewTransact ops -> do
      mrDebug 3 "Got new transactions."
      hrQueue %= Map.union (Map.fromList ops)
