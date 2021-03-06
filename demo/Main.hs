{-# LANGUAGE LambdaCase #-}

import UCap.Coord
import UCap.Demo.Config
import UCap.Lens
import UCap.Op
import UCap.Replica.Debug
import UCap.Replica.MRep
import UCap.Replica.HttpDemo

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Time.Clock
import System.Environment (getArgs)

debugReport :: Debug -> ExConf -> ExprData -> IO ()
debugReport debug conf (ss,fs) = do
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
  debug DbSetup 1 $ "Total: " ++ show total ++ " tr"
  debug DbSetup 1 $ "Throughput: " ++ show thp ++ " tr/s"
  debug DbSetup 1 $ "Avg. latency: " ++ show latn ++ " ms"

writeCsvReport :: FilePath -> Int -> ExConf -> ExprData -> String -> IO ()
writeCsvReport fp eid conf (ss,fs) kind = do
  let total = Map.size fs
      rate = exConfRate conf
      thp = 
        fromIntegral total
        / nominalDiffTimeToSeconds (exConfDuration conf)
      latn = Map.foldlWithKey f 0 fs * fromIntegral 1000
        where f t k end =
                let start = fromJust $ Map.lookup k ss
                    tk = nominalDiffTimeToSeconds (diffUTCTime end start)
                         / fromIntegral total
                in t + tk
      csvl = show eid ++ ","
             ++ kind ++ ","
             ++ show rate ++ ","
             ++ show total ++ ","
             ++ show thp ++ ","
             ++ show latn ++ "\n"
  appendFile fp csvl

main :: IO ()
main = do
  args <- getArgs
  (gc,lc,es) <- case args of
    -- [gc,lc] -> (,) <$> dhallInput (dCombinedConfig dSimpleEx) gc
    --                <*> dhallInput dLocalConfig lc
    [cc] -> dhallInput (dCombinedConfig dSimpleEx) cc
    _ -> error $ "Must call with 2 args (global, local) \
                 \or 1 arg (combined)"
  mapM_ (runExpr gc lc) (zip [0..] es)

runExpr :: Addrs -> LocalConfig -> (Int, Experiment SimpleEx) -> IO ()
runExpr addrs lc (eid,ex) = do
  let exconf = exConf ex
      sid = "store" ++ show eid
      rid = lcId lc
      -- addrs = gcNetwork gc
      trs = case exSetup ex of
              _ -> repeat $ subOp 1 >>> pure ()

  dbchan <- if anyDebug (lcDebug lc)
               then Just <$> newTChanIO
               else return Nothing
  let debug = case dbchan of
        Just c -> mkDebug (lcDebug lc)
                          (\dc s -> let s' = "=> [" ++ show eid
                                             ++ ":" ++ show dc
                                             ++ "] " ++ s
                                    in atomically . writeTChan c $ s')
        Nothing -> \_ _ _ -> return ()
  -- let debug s = case dbchan of
  --                 Just c | lcDebug lc >= 1 -> 
  --                   atomically . writeTChan c $ "=> " ++ s
  --                 _ -> return ()

  tq <- newTChanIO -- transaction queue
  ts <- newTVarIO mempty -- transaction results map
  done <- newEmptyTMVarIO -- termination notification
  shutdown <- newEmptyTMVarIO -- termination control
  let runShutdown = atomically $ putTMVar shutdown ()
  confirm <- newEmptyTMVarIO -- termination confirmation
  allReady <- newEmptyTMVarIO

  let primary = head $ Map.keys addrs

  forkFinally
    (case exSetup ex of
        TokenEx -> demoRep shutdown allReady tq ts debug rid $ 
                     HRSettings { _hsAddrs = addrs
                                , _hsInitState = 100 :: Int
                                , _hsInitCoord = mkTokenG primary
                                , _hsStoreId = sid
                                , _hsLocalId = rid
                                }
        EscrowEx n b ->
          let g = initIntEscrow b [primary] $
                    Map.fromList [(primary,(n,0))]
          in demoRep shutdown allReady tq ts debug rid $ 
               HRSettings { _hsAddrs = addrs
                          , _hsInitState = n
                          , _hsInitCoord = g
                          , _hsStoreId = sid
                          , _hsLocalId = rid
                          })

    (\case
        Right (Right (_,d),s) -> do
          debug DbSetup 2 $ "Terminated with state: " ++ show s
          debugReport debug (exConf ex) d
          case lcOutPath lc of
            Just fp ->
              writeCsvReport fp eid (exConf ex) d (show $ exSetup ex)
            Nothing -> return ()
          atomically $ putTMVar confirm ()
        Left e -> do
          debug DbSetup 1 (show e)
          atomically $ putTMVar confirm ())

  forkIO $ do
    -- Block until all replicas are active
    () <- atomically $ takeTMVar allReady
    debug DbSetup 2 "allReady!"
    t <- getCurrentTime
    let h = (tq,ts,done)
    evalStateT
      (feedLoop h exconf (Map.size addrs))
      (FeedLoopState { _flsStartTime = t
                     , _flsIndex = 0
                     , _flsTrs = trs})

  debug DbSetup 1 "Replica initialized"
  debugLoop
    dbchan
    (Map.singleton rid confirm)
    (Map.singleton rid done)
    runShutdown
