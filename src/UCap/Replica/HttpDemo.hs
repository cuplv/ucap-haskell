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

import Control.Concurrent (forkIO, forkFinally, killThread)
import Control.Concurrent.STM
import Control.Monad.Trans
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set

data HRSettings g = HRSettings
  { _hsAddrs :: Addrs
  , _hsInitState :: GState g
  , _hsInitCoord :: g
  }

makeLenses ''HRSettings

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
        , _hsInitState = 1000
        , _hsInitCoord = initIntEscrow [alphaId] $ Map.fromList
            [(alphaId, (1000,0))
            ]
        }
      scripts = Map.fromList $
        [(betaId, transactManySD_ (replicate 500 $ subOp 1))
        ,(gammaId, transactManySD_ (replicate 500 $ subOp 1))
        ]
      daemons = Map.fromList $
        [(alphaId, loopSD $ trBlock grantRequests')]
  runDemo sets scripts daemons

lockDemo :: IO ()
lockDemo = do
  let sets = HRSettings
        { _hsAddrs = addrMap
        , _hsInitState = 50
        , _hsInitCoord = mkTokenG alphaId
        }
      scripts = Map.fromList $
        [(betaId, transactManySD_ (replicate 20 $ subOp 1))
        ,(gammaId, transactManySD_ (replicate 10 $ subOp 3))
        ]
      daemons = Map.fromList $
        [(alphaId, loopSD $ trBlock grantRequests')]
  runDemo sets scripts daemons

data DebugLoop
  = Debug String
  | ScriptsDone
  | AllDone


debugLoop
  :: TChan String
  -> Map RId (TMVar ())
  -> Map RId (TMVar ())
  -> IO ()
  -> IO ()
debugLoop dbg allDone cs shutdown = do
  r <- atomically $ (Debug <$> readTChan dbg)
                    `orElse` (const ScriptsDone
                              <$> mapM_ takeTMVar cs)
                    `orElse` (const AllDone <$> mapM_ takeTMVar allDone)
  case r of
    Debug s -> putStrLn s >> debugLoop dbg allDone cs shutdown
    ScriptsDone -> shutdown >> debugLoop dbg allDone cs shutdown
    AllDone -> return ()

runDemo
  :: (HttpCS g, Show a)
  => HRSettings g
  -> Map RId (EScriptT g a)
  -> Map RId (EScriptT g ())
  -> IO ()
runDemo sets scripts daemons = do
  dbg <- newTChanIO :: IO (TChan String)
  
  let sids = Map.keys scripts
  let dids = Map.keys daemons
  let ids = sids ++ dids
  incomplete <- Map.fromList <$> mapM (\i -> (,) i <$> newEmptyTMVarIO) sids
  allDone <- Map.fromList <$> mapM (\i -> (,) i <$> newEmptyTMVarIO) ids
  dsds <- Map.fromList <$> mapM (\i -> (,) i <$> newEmptyTMVarIO) ids
  let runRep rid ic = do
        let sc = fromJust $ Map.lookup rid scripts
        let sc' = do sc
                     liftEScript . atomically $ putTMVar ic ()
                     loopSD $ trBlock grantRequests'
        let debug s = atomically . writeTChan dbg $ 
              "=>  " ++ rid ++ ": " ++ s ++ "\n"
        let shutdown = fromJust $ Map.lookup rid dsds
        demoRep' shutdown debug rid sets sc'
  let forkFin rid = do
        let mv = fromJust $ Map.lookup rid allDone
        let ic = fromJust $ Map.lookup rid incomplete
        forkFinally 
          (do atomically . writeTChan dbg $ 
                "[*] " ++ rid ++ " initialized, with state "
                ++ show (sets ^. hsInitState)
              runRep rid ic)
          (\case
              Right (Right a,s) -> atomically $ do
                -- let ic = fromJust $ Map.lookup rid incomplete
                -- putTMVar ic ()
                writeTChan dbg $ "[+] " ++ rid ++ " returned " ++ show a
                                 ++ ", with state " ++ show s
                putTMVar mv ()
              Right (Left (),s) -> atomically $ do
                -- let ic = fromJust $ Map.lookup rid incomplete
                putTMVar ic ()
                writeTChan dbg $ "[+] " ++ rid ++ " shut down, with state " ++ show s
                putTMVar mv ()
              Left e -> atomically $ do
                writeTChan dbg $ show e
                putTMVar mv ())
  let runRepD rid = do
        let sc = fromJust $ Map.lookup rid daemons
        let debug s = atomically . writeTChan dbg $
              "=>  " ++ rid ++ ": " ++ s ++ "\n"
        let shutdown = fromJust $ Map.lookup rid dsds
        demoRep' shutdown debug rid sets sc
  let forkFinD rid = do
        let mv = fromJust $ Map.lookup rid allDone
        forkFinally
          (do atomically . writeTChan dbg $
                "[*] " ++ rid ++ " initialized, with state "
                ++ show (sets ^. hsInitState)
              runRepD rid)
          (\case
              Right (Right a,s) -> atomically $ do
                writeTChan dbg $ "[+] " ++ rid ++ " returned " ++ show a
                                 ++ ", with state " ++ show s
                putTMVar mv ()
              Right (Left (),s) -> atomically $ do
                writeTChan dbg $ "[+] " ++ rid ++ " shut down, with state " ++ show s
                putTMVar mv ()
              Left e -> atomically $ do
                writeTChan dbg $ show e
                putTMVar mv ())
          -- (\(Right (a,s)) -> )
  mapM_ forkFinD dids
  mapM_ forkFin sids
  let runShutdown = atomically $ mapM_ (\m -> putTMVar m ()) dsds
  debugLoop dbg allDone incomplete runShutdown

demoRep
  :: (HttpCS g)
  => (String -> IO ()) -- ^ Debug action
  -> RId
  -> HRSettings g
  -> EScriptT g a
  -> IO (Either () a, GState g)
demoRep debug rid sets sc = do
  shutdown <- newEmptyTMVarIO
  demoRep' shutdown debug rid sets sc

demoRep'
  :: (HttpCS g)
  => TMVar () -- ^ shutdown command input
  -> (String -> IO ()) -- ^ Debug action
  -> RId
  -> HRSettings g
  -> EScriptT g a
  -> IO (Either () a, GState g)
demoRep' shutdown debug rid sets sc = do
  inbox <- newTChanIO
  -- shutdown <- newEmptyTMVarIO
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
        }
  a <- evalMRepScript' sc (sets^.hsInitState) (sets^.hsInitCoord) info
  killThread tid
  return a
