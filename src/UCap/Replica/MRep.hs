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

class (GId g ~ RId, Eq g, Eq (GEffect g), CoordSys g, Show g) => MCS g
instance (GId g ~ RId, Eq g, Eq (GEffect g), CoordSys g, Show g) => MCS g

data BMsg g e
  = BPing (Set RId) VC
  | BPong (Set RId) VC
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
              , _hrShutdownActive :: Bool
              , _hrQueue :: Map Int (Op' g)
              , _lastSentClock :: VC
              , _hrLiveNodes :: Set RId
              , _hrTrFinish :: Map Int UTCTime
              }

type Op' g = Op (GCap g) IO () ()

makeLenses ''MRepState

type Addrs = Map RId (String,Int)


data MRepInfo g e
  = MRepInfo { _hrId :: RId
             , _hrAddrs :: Addrs
             , _hrSend :: RId -> RId -> BMsg g e -> IO ()
             , _hrInbox :: TChan (TBM g e)
             , _hrDebug :: String -> IO ()
             , _hrShutdown :: TMVar ()
             , _hrGetQueue :: TChan (Int, Op' g)
             , _hrTrStart :: TVar (Map Int UTCTime)
             , _hrAllReady :: TMVar ()
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
  -> IO (Either () (a, ExprData),GState g)
evalMRepScript' sc s0 g0 info =
  let st = MRepState { _hrInitState = s0
                     , _hrCurrentState = s0
                     , _hrCoord = g0
                     , _hrDag = initThreads
                     , _hrMsgWaiting = []
                     , _hrShutdownActive = False
                     , _hrQueue = Map.empty
                     , _lastSentClock = zeroClock
                     , _hrLiveNodes = Set.singleton (info^.hrId)
                     , _hrTrFinish = Map.empty
                     }
      m = do mrPing
             -- In case effects have been missed due to slow start-up,
             -- we request a complete copy of the state from all
             -- listening peers.
             mrRequestAll
             a <- mrAwaitScript sc
             d <- mrCollectData
             return (a,d)
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
  sd <- use hrShutdownActive
  q <- use hrQueue
  mrDebug $ "Passing along " ++ show (length q) ++ " trs."
  return $ ExRd
    { _exrStore = ctx
    , _exrQueue = q
    , _exrShutdown = sd
    }

mrWriteState :: (MCS g) => ExWr g (GEffect g) -> MRepT g ()
mrWriteState (ExWr (RepCtx e mg) trm rc) = do
  mapM_ (\i -> hrQueue %= Map.delete i) rc
  mrDebug $ show rc
  if not $ null trm
     then do t <- liftIO getCurrentTime
             let f (i,TermComplete) = hrTrFinish %= Map.insert i t
             mapM_ f trm
             -- tv <- view hrTermRecords
             -- let f m = Map.unionWith (<>) m (Map.fromList trm)
             -- liftIO $ atomically $ modifyTVar tv f
             mrDebug $ show trm
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
  rid <- view hrId
  send <- view hrSend
  liftIO $ send rid i msg

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
                       mrCompareClock
                       return MsgUpdate
        Left NotCausal -> return MsgNonCausal
        Left IncompleteClock -> error $ "Failed to import from" ++ show i
    BPing lv v -> do
      -- hrLiveNodes %= Set.union lv
      mrUpdateLive lv
      -- pong <- mkPong
      let pong = mrUnicast i =<< mkPong
      -- let pong = do v1 <- mrGetClock
      --               mrUnicast i $ BPong v1
      hrDag `eitherModifying` updateClock i v >>= \case
        Left MissingEvents -> return MsgNonCausal
        Left OldValues -> pong >> return MsgOld
        Right () -> pong >> return MsgUpdate
    BPong lv v -> do
      -- hrLiveNodes %= Set.union lv
      mrUpdateLive lv
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
        Right () -> do
          v' <- mrGetClock
          hrDag `eitherModifying` updateClock rid v' >>= \case
            Right () -> updateStateVal >> return MsgUpdate
            Left MissingEvents -> error "Missing events after BComplete"
            Left OldValues -> return MsgOld
        -- Right () -> do updateClock rid (totalClock dag1)
        --                updateStateVal
        --                return MsgUpdate
        Left i2 -> error $ "BComplete error, cannot merge on " 
                           ++ show i2
    BRequest -> do
      mrDebug $ "Handle BRequest from " ++ show i
      dag <- use hrDag
      g <- use hrCoord
      mrUnicast i $ BComplete dag g
      return MsgOld

mrUpdateLive :: (MCS g) => Set RId -> MRepT g ()
mrUpdateLive lv = do
  lv' <- hrLiveNodes <%= Set.union lv
  others <- mrOtherIds
  mrDebug $ "Active nodes: " ++ show lv'
  if Set.fromList others `Set.isSubsetOf` lv'
     then do a <- view hrAllReady
             liftIO.atomically $ tryPutTMVar a ()
             return ()
     else return ()

mrCompareClock :: (MCS g) => MRepT g ()
mrCompareClock = do
  v1 <- mrGetClock
  v2 <- use lastSentClock
  dag <- use hrDag
  mrDebug $ "Clock diff at " ++ show (aggDiff v1 v2)
  mrDebug $ "Dag size at " ++ show (sizeVT dag)
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
    -- mrPrintReport
    return a
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
  | MrNewTransact (Int, Op' g)

mrWaitChange :: (MCS g) => MRepT g ()
mrWaitChange = do
  chan <- view hrInbox
  sd <- view hrShutdown
  tq <- view hrGetQueue
  let stm = 
        (MrGotMsg <$> readTChan chan)
        `orElse` (MrShutdown <$ takeTMVar sd)
        `orElse` (MrNewTransact <$> readTChan tq)
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
    MrNewTransact (n,op) -> do
      mrDebug "Got new transaction."
      hrQueue %= Map.insert n op
