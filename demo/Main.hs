{-# LANGUAGE LambdaCase #-}

import UCap.Demo.Config
import UCap.Lens
import UCap.Op
import UCap.Replica.MRep
import UCap.Replica.HttpDemo

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Time.Clock
import System.Environment (getArgs)

mkReport :: Config -> ExprData -> String
mkReport conf (ss,fs) =
  let total = Map.size fs
      thp = 
        fromIntegral total
        / nominalDiffTimeToSeconds (exConfDuration $ exprSettings conf)
      latn = Map.foldlWithKey f 0 fs * fromIntegral 1000
        where f t k end =
                let start = fromJust $ Map.lookup k ss
                    tk = nominalDiffTimeToSeconds (diffUTCTime end start)
                         / fromIntegral total
                in t + tk
  in "Total: " ++ show total ++ " tr\n"
     ++ "Throughput: " ++ show thp ++ " tr/s\n"
     ++ "Avg. latency: " ++ show latn ++ " ms"

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
  let debug s = if debugLvl config > 0
                   then atomically . writeTChan dbg $ "=> " ++ s
                   else return ()
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
        Right (Right (_,d),s) -> do
          debug $ "Terminated with state: " ++ show s
          putStr $ mkReport config d
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
