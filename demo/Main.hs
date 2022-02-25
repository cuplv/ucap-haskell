{-# LANGUAGE LambdaCase #-}

import UCap.Demo.Config
import UCap.Lens
import UCap.Op
import UCap.Replica.HttpDemo

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.State
import qualified Data.Map as Map
import Data.Time.Clock
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  config <- inputConfig (head args)
  let sets = commonSettings config
      exconf = exprSettings config
      rid = localId config

  let trs = case repRole config of
              "idle" -> []
              "active" -> repeat (subOp 1 >>> pure ())
              s -> error $ "No role " ++ show s

  dbg <- newTChanIO :: IO (TChan String) -- debug log
  let debug s = atomically . writeTChan dbg $ "=> " ++ s
  tq <- newTChanIO -- transaction queue
  ts <- newTVarIO mempty -- transaction results map
  done <- newEmptyTMVarIO -- termination notification
  shutdown <- newEmptyTMVarIO -- termination control
  let runShutdown = atomically $ putTMVar shutdown ()
  confirm <- newEmptyTMVarIO -- termination confirmation
  allReady <- newEmptyTMVarIO

  forkFinally
    (demoRep shutdown allReady tq ts debug rid sets)
    (\case
        Right (_,s) -> do
          debug $ "Terminated with state: " ++ show s
          atomically $ putTMVar confirm ()
        Left e -> do
          debug (show e)
          atomically $ putTMVar confirm ())

  forkIO $ do
    -- Block until all replicas are active
    () <- atomically $ takeTMVar allReady
    debug "allReady!"
    t <- getCurrentTime
    let h = (tq,ts,done)
    evalStateT
      (feedLoop h exconf)
      (FeedLoopState { _flsStartTime = t
                     , _flsIndex = 0
                     , _flsTrs = trs})

  debugLoop
    dbg
    (Map.singleton rid confirm)
    (Map.singleton rid done)
    runShutdown
