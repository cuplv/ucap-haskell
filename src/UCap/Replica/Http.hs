{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module UCap.Replica.Http
  ( HttpCS
  , mkSenders
  , mkListener
  ) where

import UCap.Coord
import UCap.Replica.Debug
import UCap.Replica.MRep

import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.STM
import qualified Control.Exception.Base as Exception
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Network.HTTP.Client as Client
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import qualified Data.ByteString.Lazy.Char8 as BS

class
  ( MCS g
  , ToJSON g
  , ToJSON (GCap g)
  , ToJSON (GEffect g)
  , FromJSON g
  , FromJSON (GCap g)
  , FromJSON (GEffect g)
  , Show g
  , Show (GCap g)
  , Show (GEffect g)
  , Show (GState g)
  ) => HttpCS g
instance
  ( MCS g
  , ToJSON g
  , ToJSON (GCap g)
  , ToJSON (GEffect g)
  , FromJSON g
  , FromJSON (GCap g)
  , FromJSON (GEffect g)
  , Show g
  , Show (GCap g)
  , Show (GEffect g)
  , Show (GState g)
  ) => HttpCS g

{-| 'mapM_' specialized to lists, to avoid type ambiguity when decoding
  from HTTP request bodies. -}
mapM_' :: (Monad m) => (a -> m b) -> [a] -> m ()
mapM_' = mapM_

msgGetter
  :: (HttpCS g)
  => TChan (TBM' g)
  -> Debug
  -> String -- ^ Store ID
  -> Application
msgGetter chan debug sid request respond = do
  body <- strictRequestBody request
  case decode body of
    Just (msid,source,msgs) -> do
      mapM_'
        (\msg -> debug DbTransport (dbLevel msg) $ "RECV(" ++ source ++ ")"
                                                   ++ show msg)
        msgs
      r <- respond $ responseLBS status200 [] ""
      if msid == sid
         then atomically $ mapM_' (\msg -> writeTChan chan (source,msg)) msgs
         else debug DbTransport 2 $ "Dropping msg for store " ++ show msid
      return r
    Nothing -> do
      debug DbTransport 1 $ "Failed to decode msg"
      respond $ responseLBS status400 [] ""

mkListener
  :: (HttpCS g)
  => Int
  -> TChan (TBM' g)
  -> Debug
  -> String -- ^ Store ID
  -> IO ()
mkListener port chan debug sid = do
  runSettings (setPort port $ defaultSettings) (msgGetter chan debug sid)

dbLevel :: BMsg' g -> Int
dbLevel = \case
  BPing _ _ -> 5
  BPong _ _ -> 5
  BCoord _ _ -> 4
  _ -> 3

sendMsg
  :: (HttpCS g)
  => Client.Manager
  -> Debug -- ^ Debug
  -> String -- ^ Store ID
  -> RId -- ^ Source ID
  -> RId -- ^ Target ID
  -> (String,Int) -- ^ Address
  -> [BMsg' g]
  -> Int -- ^ Retries
  -> IO ()
sendMsg man debug sid source target (host,port) msgs rtr = do
  if length msgs > 1
     then debug DbTransport 2 $ "Sending " ++ show (length msgs)
                                ++ " msgs to " ++ target
     else return ()
  initialRequest <- Client.parseRequest $
                      "http://" ++ host ++ ":" ++ show port ++ "/"
  let req = initialRequest 
              { Client.method = "POST"
              , Client.requestBody =
                  Client.RequestBodyLBS $ encode (sid,source,msgs)
              }
  mapM_ (\m -> debug DbTransport (dbLevel m) $ 
                 "SEND(" ++ target ++ ") " ++ show m)
        msgs
  Exception.catch (Client.httpLbs req man >> return ()) $ \e ->
    case e of
      Client.HttpExceptionRequest _ (Client.ConnectionFailure _) -> 
        case findCoord msgs of
          [] -> debug DbTransport 2 $ "failed send to " ++ show target
                                      ++ ", msgs " ++ show msgs
          _ -> debug DbTransport 1 $ "failed coord to " ++ show target
                                     ++ ", msgs " ++ show msgs
        -- debug DbTransport 1 $ "failed send to " ++ show target
        --                       ++ ", msg " ++ show msgs
      Client.HttpExceptionRequest _ (Client.InternalException _) -> do
        debug DbTransport 1 $ "internal exception on send to "
                              ++ show target ++ ", msgs " ++ show msgs
        if rtr > 0
           then do debug DbTransport 1 $ "Remaining retries: " ++ show (rtr - 1)
                   sendMsg man debug sid source target (host,port) msgs (rtr - 1)
           else return ()
      e -> error $ "unhandled http-client exception: " ++ show e
  return ()

findCoord = filter (\m -> case m of
                            BCoord _ _ -> True
                            _ -> False)

{-| Read 0 or more elements from a 'TChan'.  This never retries. -}
readMany :: TChan a -> STM [a]
readMany chan = tryReadTChan chan >>= \case
  Just a -> (a :) <$> readMany chan
  Nothing -> return []

{-| Read 1 or more elements from a 'TChan'.  This will retry when there
  are no elements in the channel. -}
readMany1 :: TChan a -> STM [a]
readMany1 chan = do
  m1 <- readTChan chan
  ms <- readMany chan
  return (m1:ms)

scanMsgs :: [Either () a] -> (Bool,[a])
scanMsgs (Right a : ms) = (a :) <$> scanMsgs ms
scanMsgs (Left () : _) = (True,[])
scanMsgs [] = (False,[])

sendLoop
  :: (HttpCS g)
  => Client.Manager
  -> Debug
  -> String -- ^ Store ID
  -> RId -- ^ Source ID
  -> RId -- ^ Target ID
  -> (String,Int) -- ^ Target address
  -> TChan (Either () (BMsg' g)) -- ^ Outbox
  -> TMVar () -- ^ Done
  -> IO ()
sendLoop man debug sid source target addr outbox done = do
  ms <- atomically $ readMany1 outbox
  let (eom,ms') = scanMsgs ms
  sendMsg man debug sid source target addr ms' 5
  if eom
     then atomically $ putTMVar done ()
     else sendLoop man debug sid source target addr outbox done


mkSenders
  :: (HttpCS g)
  => Debug
  -> String -- ^ Store ID
  -> RId -- ^ Replica ID
  -> Map RId (String,Int)
  -> IO (Map RId (TChan (Either () (BMsg' g)), TMVar ()))
mkSenders debug sid rid addrs = do
  man <- Client.newManager Client.defaultManagerSettings
  let f i addr = do
        outbox <- newTChanIO
        done <- newEmptyTMVarIO
        tid <- forkIO $ sendLoop man debug sid rid i addr outbox done
        return (outbox, done)
  Map.traverseWithKey f addrs
