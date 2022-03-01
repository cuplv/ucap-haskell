{-# LANGUAGE LambdaCase #-}

import UCap.Coord
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

mkReport :: ExConf -> ExprData -> String
mkReport conf (ss,fs) =
  let total = Map.size fs
      thp = 
        fromIntegral total
        / nominalDiffTimeToSeconds (exConfDuration conf)
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
  (gc,lc) <- case args of
               [gc,lc] -> (,) <$> dhallInput (dGlobalConfig dSimpleEx) gc
                              <*> dhallInput dLocalConfig lc
               [cc] -> dhallInput (dCombinedConfig dSimpleEx) cc
               _ -> error $ "Must call with 2 args (global, local) \
                            \or 1 arg (combined)"
  let exconf = gcExConf gc
      rid = lcId lc
      addrs = gcNetwork gc
      trs = case gcExSetup gc of
              _ -> repeat $ subOp 1 >>> pure ()

  dbchan <- if lcDebug lc > 0
               then Just <$> newTChanIO
               else return Nothing
  let debug s = case dbchan of
                  Just c | lcDebug lc >= 1 -> 
                    atomically . writeTChan c $ "=> " ++ s
                  _ -> return ()

  tq <- newTChanIO -- transaction queue
  ts <- newTVarIO mempty -- transaction results map
  done <- newEmptyTMVarIO -- termination notification
  shutdown <- newEmptyTMVarIO -- termination control
  let runShutdown = atomically $ putTMVar shutdown ()
  confirm <- newEmptyTMVarIO -- termination confirmation
  allReady <- newEmptyTMVarIO

  forkFinally
    (case gcExSetup gc of
        TokenEx i -> demoRep shutdown allReady tq ts debug rid $ 
                       HRSettings addrs (100::Int) (mkTokenG i)
        EscrowEx i n -> 
          let g = initIntEscrow [i] $ Map.fromList [(i,(n,0))]
          in demoRep shutdown allReady tq ts debug rid $ 
               HRSettings addrs n g)

    (\case
        Right (Right (_,d),s) -> do
          debug $ "Terminated with state: " ++ show s
          putStr $ mkReport (gcExConf gc) d
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
      (feedLoop h exconf (Map.size addrs))
      (FeedLoopState { _flsStartTime = t
                     , _flsIndex = 0
                     , _flsTrs = trs})

  debugLoop
    dbchan
    (Map.singleton rid confirm)
    (Map.singleton rid done)
    runShutdown
