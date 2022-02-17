{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

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
import Data.Maybe (fromJust)

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
        , _hsInitState = 50
        , _hsInitCoord = initIntEscrow [alphaId] $ Map.fromList
            [(alphaId, (50,0))
            ]
        }
      scripts = Map.fromList $
        [(alphaId, loopBlock grantRequests')
        ,(betaId, transactMany_ (replicate 20 $ subOp 1))
        ,(gammaId, transactMany_ (replicate 10 $ subOp 3))
        ]
  runDemo sets scripts

debugLoop :: TChan String -> Map RId (TMVar ()) -> IO ()
debugLoop dbg allDone = do
  r <- atomically $ (Just <$> readTChan dbg)
                    `orElse` (const Nothing <$> mapM_ takeTMVar allDone)
  case r of
    Just s -> putStrLn s >> debugLoop dbg allDone
    Nothing -> return ()

runDemo
  :: (HttpCS g, Show a)
  => HRSettings g
  -> Map RId (ScriptT g IO a)
  -> IO ()
runDemo sets scripts = do
  dbg <- newTChanIO :: IO (TChan String)
  let ids = Map.keys scripts
  allDone <- Map.fromList <$> mapM (\i -> (,) i <$> newEmptyTMVarIO) ids
  let runRep rid = do
        let sc = fromJust $ Map.lookup rid scripts
        let debug s = atomically . writeTChan dbg $ 
              "=>  " ++ rid ++ ": " ++ s ++ "\n"
        demoRep debug rid sets sc
  let forkFin rid = do
        let mv = fromJust $ Map.lookup rid allDone
        forkFinally 
          (do atomically . writeTChan dbg $ 
                "[*] " ++ rid ++ " initialized, with state "
                ++ show (sets ^. hsInitState)
              runRep rid)
          (\(Right (a,s)) -> atomically $ do
             writeTChan dbg $ "[+] " ++ rid ++ " returned " ++ show a
                              ++ ", with state " ++ show s
             putTMVar mv ())
  mapM_ forkFin ids
  debugLoop dbg allDone

demoRep
  :: (HttpCS g)
  => (String -> IO ()) -- ^ Debug action
  -> RId
  -> HRSettings g
  -> ScriptT g IO a
  -> IO (a, GState g)
demoRep debug rid sets sc = do
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
        }
  a <- evalMRepScript' sc (sets^.hsInitState) (sets^.hsInitCoord) info
  killThread tid
  return a
