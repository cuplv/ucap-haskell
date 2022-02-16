module UCap.Replica.HttpDemo where

import UCap.Coord
import UCap.Lens
import UCap.Op
import UCap.Replica.Http
import UCap.Replica.MRep
import UCap.Replica.Script
import UCap.Replica.Transact

import Control.Concurrent (forkIO, forkFinally, killThread)
import Control.Concurrent.STM
import Control.Monad.Trans
import Data.Map (Map)
import qualified Data.Map as Map

alphaId = "alpha"
alphaPort = 8090
betaId = "beta"
betaPort = 8091

localhost = "127.0.0.1"

addrMap = Map.fromList
  [(alphaId, (localhost,alphaPort))
  ,(betaId, (localhost,betaPort))
  ]

type G = IntEscrow RId

initState = 50

initCoord = initIntEscrow [alphaId] $ Map.fromList
  [(alphaId, (50,0))
  ]

-- dbPrint :: TChan String -> IO ()
-- dbPrint chan = do
--   s <- atomically (readTChan chan)
--   putStrLn s
--   dbPrint chan

debugLoop :: TChan String -> TMVar () -> IO ()
debugLoop dbg done = do
  r <- atomically $ (Just <$> readTChan dbg)
                    `orElse` (const Nothing <$> takeTMVar done)
  case r of
    Just s -> putStrLn s >> debugLoop dbg done
    Nothing -> return ()

runDemo :: IO ()
runDemo = do
  dbg <- newTChanIO :: IO (TChan String)
  betaDone <- newEmptyTMVarIO
  -- dt <- forkIO $ dbPrint dbg
  a <- forkIO $ demoAlpha dbg
  forkFinally (demoBeta dbg) (\_ -> atomically $ putTMVar betaDone ())
  debugLoop dbg betaDone
  killThread a

demoAlpha :: TChan String -> IO ()
demoAlpha dchan = do
  let debug s = atomically $ writeTChan dchan ("=> " ++ alphaId ++ ": " ++ s ++ "\n")
  send <- mkSender debug addrMap
  inbox <- newTChanIO :: IO (TChan (TBM' G))
  tid <- forkIO $ mkListener alphaPort inbox debug
  let info = MRepInfo
        { _hrId = alphaId
        , _hrOtherIds = [betaId]
        , _hrSend = send
        , _hrInbox = inbox
        , _hrDebug = debug
        }
      sc = loopBlock $ do
             s <- checkState
             lift . liftIO $ debug $ "state is " ++ show (s^.rsStore)
             grantRequests'
  evalMRepScript sc initState initCoord info
  killThread tid

demoBeta :: TChan String -> IO ()
demoBeta dchan = do
  let debug s = atomically $ writeTChan dchan ("=> " ++ betaId ++ ": " ++ s ++ "\n")
  send <- mkSender debug addrMap
  inbox <- newTChanIO :: IO (TChan (TBM' G))
  tid <- forkIO $ mkListener betaPort inbox debug
  let info = MRepInfo
        { _hrId = betaId
        , _hrOtherIds = [alphaId]
        , _hrSend = send
        , _hrInbox = inbox
        , _hrDebug = debug
        }
      sc = do
        s0 <- view rsStore <$> readState
        liftScript $ debug $ "init with state " ++ show s0
        transactMany_ (replicate 5 $ subOp 1)
        s1 <- view rsStore <$> readState
        liftScript $ debug $ "finished with state " ++ show s1
  evalMRepScript sc initState initCoord info
  killThread tid
