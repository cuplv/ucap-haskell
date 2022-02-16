module UCap.Replica.HttpDemo where

import UCap.Coord
import UCap.Lens
import UCap.Op
import UCap.Replica.Http
import UCap.Replica.MRep
import UCap.Replica.Script
import UCap.Replica.Transact

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM
import Data.Map (Map)
import qualified Data.Map as Map

alphaId = "alpha"
alphaPort = 8090
betaId = "beta"
betaPort = 8091

localhost = "127.0.0.1"

addrMap = Map.fromList
  [(alphaId, (localhost,alphaPort))
  ,(betaId, (localhost,alphaPort))
  ]

type G = IntEscrow RId

initState = 50

initCoord = initIntEscrow [alphaId] $ Map.fromList
  [(alphaId, (0,50))
  ,(betaId, (0,0))
  ]

runDemo = undefined

demoAlpha :: IO ()
demoAlpha = do
  send <- mkSender addrMap
  inbox <- newTChanIO :: IO (TChan (TBM' G))
  tid <- forkIO $ mkListener alphaPort inbox
  let info = MRepInfo
        { _hrId = alphaId
        , _hrOtherIds = [betaId]
        , _hrSend = send
        , _hrInbox = inbox
        }
      sc = loopBlock grantRequests'
  evalMRepScript sc initState initCoord info
  killThread tid

demoBeta :: IO ()
demoBeta = do
  send <- mkSender addrMap
  inbox <- newTChanIO :: IO (TChan (TBM' G))
  tid <- forkIO $ mkListener betaPort inbox
  let info = MRepInfo
        { _hrId = betaId
        , _hrOtherIds = [alphaId]
        , _hrSend = send
        , _hrInbox = inbox
        }
      sc = do
        s0 <- view rsStore <$> readState
        liftScript $ print $ "Beta: Init with state " ++ show s0
        transactMany_ (replicate 10 $ subOp 1)
        s1 <- view rsStore <$> readState
        liftScript $ print $ "Beta: Finished with state " ++ show s1
  evalMRepScript sc initState initCoord info
  killThread tid
