{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module UCap.Replica.HttpDemo where

import UCap.Coord
import UCap.Domain.Int
import UCap.Lens
import UCap.Op
import UCap.Replica.Debug
import UCap.Replica.Http
import UCap.Replica.MRep
import UCap.Replica.EScript
import UCap.Replica.Script
import UCap.Replica.Transact

import Control.Concurrent (forkIO, forkFinally, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Monad.State
import Control.Monad.Trans
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock
import System.IO (hPutStrLn, stderr)

data HRSettings g = HRSettings
  { _hsAddrs :: Addrs
  , _hsInitState :: GState g
  , _hsInitCoord :: g
  , _hsStoreId :: String
  , _hsLocalId :: RId
  }

makeLenses ''HRSettings

data FeedLoopState a
  = FeedLoopState
      { _flsStartTime :: UTCTime
      , _flsIndex :: Int
      , _flsTrs :: [a]
      }

makeLenses ''FeedLoopState

alphaId = "alpha"
alphaPort = 8090
betaId = "beta"
betaPort = 8091
gammaId = "gamma"
gammaPort = 8092

localhost = "127.0.0.1"

addrMap = Map.fromList
  [(alphaId, (localhost,alphaPort))
  ,(betaId, (localhost,betaPort))
  ,(gammaId, (localhost,gammaPort))
  ]

data ExConf
  = ExConf { exConfRate :: Double -- ^ Transactions per second
           , exConfDuration :: NominalDiffTime
           }

data DebugLoop
  = Debug String
  | ScriptsDone
  | AllDone

feedLoop
  :: (TQueue (Int, a), TVar (Map Int UTCTime), TMVar ())
  -> ExConf
  -> Int
  -> StateT (FeedLoopState a) IO ()
feedLoop (tq,ts,done) conf peerCount = do
  idx <- use flsIndex
  t0 <- use flsStartTime
  tNow <- liftIO getCurrentTime
  let peerRate = exConfRate conf / fromIntegral peerCount
  let secElapsed = nominalDiffTimeToSeconds $ diffUTCTime tNow t0
  let newIdx = floor (toRational peerRate * toRational secElapsed)
  let burst = newIdx - idx
  let tids = [idx..(newIdx - 1)]
  trs <- use flsTrs
  let (now,later) = List.splitAt burst trs
  if length tids /= burst
     then error $ "feeder index update error: "
                  ++ show tids ++ " vs. " ++ show burst
                  ++ " and " ++ show newIdx ++ " vs. " ++ show idx
     else return ()
  flsIndex .= newIdx
  flsTrs .= later

  let reqOffset tid = realToFrac $ fromIntegral tid / peerRate
      calcTime tid =
        let t' = (addUTCTime (reqOffset tid) t0)
        in if tNow < t'
              then error $ "tr requested too early: "
                           ++ show tNow ++ " vs. " ++ show t'
              else (tid, t')

  liftIO.atomically $ modifyTVar ts (<> Map.fromList (map calcTime tids))
  liftIO . atomically $ mapM_ (writeTQueue tq) (zip tids now)
  liftIO $ threadDelay 1000

  t2 <- liftIO getCurrentTime
  if addUTCTime (exConfDuration conf) t0 <= t2
     then do liftIO . atomically $ putTMVar done ()
             return ()
     else feedLoop (tq,ts,done) conf peerCount

debugLoop
  :: Maybe (TChan String)
  -> Map RId (TMVar ())
  -> Map RId (TMVar ())
  -> IO ()
  -> IO ()
debugLoop dbchan sdConfirm trDone shutdown = do
  let getDebug = case dbchan of
                   Just c -> Debug <$> readTChan c
                   Nothing -> retry
  r <- atomically $ getDebug
                    `orElse` (const ScriptsDone
                              <$> mapM_ takeTMVar trDone)
                    `orElse` (const AllDone
                              <$> mapM_ takeTMVar sdConfirm)
  case r of
    Debug s -> do
      hPutStrLn stderr s
      debugLoop dbchan sdConfirm trDone shutdown
    ScriptsDone -> do
      shutdown
      debugLoop dbchan sdConfirm trDone shutdown
    AllDone -> return ()

demoRep
  :: (HttpCS g)
  => TMVar () -- ^ shutdown command input
  -> TMVar () -- ^ All-ready notifier
  -> TQueue (Int, Op' g) -- ^ Transaction queue
  -> TVar (Map Int UTCTime) -- ^ Transaction start times
  -> Debug -- ^ Debug action
  -> RId
  -> HRSettings g
  -> IO (Either () ((), ExprData), (GState g, String))
demoRep shutdown allReady tq tstatus debug rid sets = do
  inbox <- newTChanIO
  senders <- mkSenders
               debug
               (sets ^. hsStoreId)
               (sets ^. hsLocalId)
               (sets ^. hsAddrs)
  let send target msg = do
        let chan = fst (senders Map.! target)
        atomically $ writeTChan chan (Right msg)
      eom = mapM_ (\c -> atomically $ writeTChan (fst c) (Left ())) senders
  let port = case Map.lookup rid (sets ^. hsAddrs) of
               Just (_,p) -> p
               Nothing -> error $ rid ++ " has no port"
  tid <- forkIO $ mkListener port inbox debug (sets ^. hsStoreId)
  let info = MRepInfo
        { _hrId = rid
        , _hrAddrs = sets ^. hsAddrs
        , _hrSend = send
        , _hrEOM = eom
        , _hrInbox = inbox
        , _hrDebug = debug
        , _hrShutdown = shutdown
        , _hrGetQueue = tq
        , _hrTrStart = tstatus
        , _hrAllReady = allReady
        }
  (a,(s,g)) <- evalMRepScript'
                 (transactQueue debug)
                 (sets^.hsInitState)
                 (sets^.hsInitCoord)
                 info
  debug DbTransport 2 $ "Waiting for senders to finish"
  atomically $ mapM_ (takeTMVar . snd) senders
  killThread tid
  return (a,(s, show g))
