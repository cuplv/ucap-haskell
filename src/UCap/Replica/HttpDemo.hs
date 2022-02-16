module UCap.Replica.HttpDemo where

import UCap.Coord
import UCap.Replica.MRep
import UCap.Replica.Http

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM

alphaPort = 8090

betaPort = 8091

type G = IntEscrow RId

demoAlpha :: IO ()
demoAlpha = do
  inbox <- newTChanIO :: IO (TChan (TBM' G))
  tid <- forkIO $ mkListener alphaPort inbox
  killThread tid
