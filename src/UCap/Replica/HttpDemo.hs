{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module UCap.Replica.HttpDemo where

import UCap.Coord
import UCap.Domain.Int
import UCap.Lens
import UCap.Op
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

data HRSettings g = HRSettings
  { _hsAddrs :: Addrs
  , _hsInitState :: GState g
  , _hsInitCoord :: g
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

escrowDemo :: IO ()
escrowDemo = do
  let sets = HRSettings
        { _hsAddrs = addrMap
        , _hsInitState = 100000
        , _hsInitCoord = initIntEscrow [alphaId] $ Map.fromList
            [(alphaId, (100000,0))
            ]
        }
      trs = repeat (subOp 1 >>> pure ())
      scripts = [(betaId, trs), (gammaId, trs)]
      daemons = [alphaId]
  runDemo sets scripts daemons

lockDemo :: IO ()
lockDemo = do
  let sets :: HRSettings (TokenG String IntC)
      sets = HRSettings
        { _hsAddrs = addrMap
        , _hsInitState = 50
        , _hsInitCoord = mkTokenG alphaId
        }
      trs = repeat (subOp 1 >>> pure ())
      scripts = [(betaId, trs), (gammaId, trs)]
      daemons = [alphaId]
  runDemo sets scripts daemons

data ExConf
  = ExConf { exConfRate :: Double -- ^ Transactions per second
           , exConfDuration :: NominalDiffTime
           }

data DebugLoop
  = Debug String
  | ScriptsDone
  | AllDone


feedLoop
  :: (TChan (Int, a), TVar (Map Int UTCTime), TMVar ())
  -> ExConf
  -> Int
  -> StateT (FeedLoopState a) IO ()
feedLoop (tq,ts,done) conf peerCount = do
  idx <- use flsIndex
  t0 <- use flsStartTime
  t <- liftIO getCurrentTime
  let peerRate = exConfRate conf / fromIntegral peerCount
  let secElapsed = nominalDiffTimeToSeconds $ diffUTCTime t t0
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

  liftIO.atomically $ modifyTVar ts (<> Map.fromList (zip tids $ repeat t))
  liftIO . atomically $ mapM_ (writeTChan tq) (zip tids now)
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
    Debug s -> putStrLn s >> debugLoop dbchan sdConfirm trDone shutdown
    ScriptsDone -> shutdown >> debugLoop dbchan sdConfirm trDone shutdown
    AllDone -> return ()

runDemo
  :: (HttpCS g)
  => HRSettings g
  -> [(RId, [Op' g])] -- ^ Transactors
  -> [RId] -- ^ Idlers
  -> IO ()
runDemo sets ops idlers = do
  dbg <- newTChanIO :: IO (TChan String)

  let transactors = map fst ops
      ids = transactors ++ idlers
      mkTrHandle i = do tq <- newTChanIO -- transaction queue
                        ts <- newTVarIO Map.empty -- tr results map
                        done <- newEmptyTMVarIO -- completion notifier
                        return (i, (tq,ts,done))
  trHandles <- Map.fromList <$> mapM mkTrHandle transactors
  sdHandles <- Map.fromList <$> mapM (\i -> (,) i <$> newEmptyTMVarIO) ids
  sdConfirm <- Map.fromList <$> mapM (\i -> (,) i <$> newEmptyTMVarIO) ids

  let runRep rid = do
        let debug s = atomically . writeTChan dbg $ 
              "=>  " ++ rid ++ ": " ++ s ++ "\n"
        let shutdown = fromJust $ Map.lookup rid sdHandles
        (tq,ts) <- case Map.lookup rid trHandles of
                     Just (tq,ts,_) -> return (tq,ts)
                     Nothing -> (,) <$> newTChanIO <*> newTVarIO Map.empty
        demoRep shutdown undefined tq ts debug rid sets

  let forkFin rid = do
        let mv = fromJust $ Map.lookup rid sdConfirm
        forkFinally 
          (do atomically . writeTChan dbg $ 
                "[*] " ++ rid ++ " initialized, with state "
                ++ show (sets ^. hsInitState)
              runRep rid)
          (\case
              Right (Right ((),d),s) -> atomically $ do
                writeTChan dbg $ "[+] " ++ rid ++ " returned,"
                                 ++ " with state " ++ show s
                putTMVar mv ()
              Right (Left (),s) -> atomically $ do
                writeTChan dbg $ "[+] " ++ rid
                                 ++ " shut down, with state " ++ show s
                putTMVar mv ()
              Left e -> atomically $ do
                writeTChan dbg $ show e
                putTMVar mv ())

  mapM_ forkFin ids

  let forkFeeder (rid,trs) = do
        let h = fromJust $ Map.lookup rid trHandles
        let conf = ExConf
              { exConfRate = 10
              , exConfDuration = secondsToNominalDiffTime 5
              }
        t <- getCurrentTime
        evalStateT (feedLoop h conf 3)
          (FeedLoopState
             { _flsStartTime = t
             , _flsIndex = 0
             , _flsTrs = trs
             })

  mapM_ (forkIO . forkFeeder) ops

  let runShutdown = atomically $ mapM_ (\m -> putTMVar m ()) sdHandles
  debugLoop (Just dbg) sdConfirm (Map.map (view _3) trHandles) runShutdown

demoRep
  :: (HttpCS g)
  => TMVar () -- ^ shutdown command input
  -> TMVar () -- ^ All-ready notifier
  -> TChan (Int, Op' g) -- ^ Transaction queue
  -> TVar (Map Int UTCTime) -- ^ Transaction start times
  -> (String -> IO ()) -- ^ Debug action
  -> RId
  -> HRSettings g
  -> IO (Either () ((), ExprData), GState g)
demoRep shutdown allReady tq tstatus debug rid sets = do
  inbox <- newTChanIO
  send <- mkSender debug (sets ^. hsAddrs)
  let port = case Map.lookup rid (sets ^. hsAddrs) of
               Just (_,p) -> p
               Nothing -> error $ rid ++ " has no port"
  tid <- forkIO $ mkListener port inbox debug
  let info = MRepInfo
        { _hrId = rid
        , _hrAddrs = sets ^. hsAddrs
        , _hrSend = send
        , _hrInbox = inbox
        , _hrDebug = debug
        , _hrShutdown = shutdown
        , _hrGetQueue = tq
        , _hrTrStart = tstatus
        , _hrAllReady = allReady
        }
  a <- evalMRepScript'
         (transactQueue debug)
         (sets^.hsInitState)
         (sets^.hsInitCoord)
         info
  killThread tid
  return a
