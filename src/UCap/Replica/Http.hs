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
import Data.Map
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

msgGetter
  :: (HttpCS g)
  => TChan (TBM' g)
  -> Debug
  -> Application
msgGetter chan debug request respond = do
  body <- strictRequestBody request
  case decode body of
    Just tbm -> do
      let src = fst tbm
          msg = snd tbm
          dbl = case msg of
                  BPing _ _ -> 3
                  BPong _ _ -> 3
                  BCoord _ _ -> 2
                  _ -> 1
      debug DbTransport dbl $ "RECV(" ++ src ++ ")" ++ show msg
      -- case msg of
      --   BPing _ _ -> return ()
      --   BPong _ _ -> return ()
      --   BCoord _ _ _ -> undefined
      --   _ -> debug 1 $ "RECV(" ++ src ++ ") " ++ show msg

      -- Complete HTTP response before sending msg to the main loop.
      -- The main loop may terminate when it receives the message,
      -- which could interrupt an incomplete response.
      r <- respond $ responseLBS status200 [] ""
      atomically (writeTChan chan tbm)
      return r
    Nothing -> respond $ responseLBS status400 [] ""

mkListener
  :: (HttpCS g)
  => Int
  -> TChan (TBM' g)
  -> Debug
  -> IO ()
mkListener port chan debug = do
  runSettings (setPort port $ defaultSettings) (msgGetter chan debug)

dbLevel :: BMsg' g -> Int
dbLevel = \case
  BPing _ _ -> 3
  BPong _ _ -> 3
  BCoord _ _ -> 2
  _ -> 1

sendMsg
  :: (HttpCS g)
  => Client.Manager
  -> Debug -- ^ Debug
  -> String -- ^ Store ID
  -> RId -- ^ Source ID
  -> RId -- ^ Target ID
  -> (String,Int) -- ^ Address
  -> [BMsg' g]
  -> IO ()
sendMsg man debug sid source target (host,port) msgs = do
  -- addr <- case Map.lookup dst addrs of
  --           Just (addr,port) -> return $ "http://" ++ addr ++ ":" ++ show port ++ "/"
  --           Nothing -> error $ show dst ++ " has no network address"
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
  -- debug DbTransport dbl $ "SEND(" ++ dst ++ ") " ++ show msg
  -- case msg of
  --   BPing _ _ -> return ()
  --   BPong _ _ -> return ()
  --   _ -> debug $ "SEND(" ++ dst ++ ") " ++ show msg
  Exception.catch (Client.httpLbs req man >> return ()) $ \e ->
    case e of
      Client.HttpExceptionRequest _ (Client.ConnectionFailure _) -> 
        debug DbTransport 1 $ "failed send to " ++ show target
                              ++ ", msg " ++ show msgs
      Client.HttpExceptionRequest _ (Client.InternalException _) ->
        debug DbTransport 1 $ "internal exception on send to "
                              ++ show target ++ ", msg " ++ show msgs
      e -> error $ "unhandled http-client exception: " ++ show e
  return ()

readMany :: TChan a -> STM [a]
readMany chan = tryReadTChan chan >>= \case
  Just a -> (a :) <$> readMany chan
  Nothing -> return []

sendLoop
  :: (HttpCS g)
  => Client.Manager
  -> Debug
  -> String -- ^ Store ID
  -> RId -- ^ Source ID
  -> RId -- ^ Target ID
  -> (String,Int) -- ^ Target address
  -> TChan (BMsg' g) -- ^ 
  -> IO ()
sendLoop man debug sid source target addr outbox = do
  m1 <- atomically $ readTChan outbox
  ms <- atomically $ readMany outbox
  sendMsg man debug sid source target addr (m1:ms)
  sendLoop man debug sid source target addr outbox

mkSenders
  :: (HttpCS g)
  => Debug
  -> String -- ^ Store ID
  -> RId -- ^ Replica ID
  -> Map RId (String,Int)
  -> IO (Map RId (TChan (BMsg' g), ThreadId))
mkSenders debug sid rid addrs = do
  man <- Client.newManager Client.defaultManagerSettings
  let f i addr = do
        outbox <- newTChanIO
        tid <- forkIO $ sendLoop man debug sid rid i addr outbox
        return (outbox, tid)
  Map.traverseWithKey f addrs
