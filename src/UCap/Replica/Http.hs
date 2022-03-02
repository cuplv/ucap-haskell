{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module UCap.Replica.Http
  ( HttpCS
  , mkSender
  , mkListener
  ) where

import UCap.Coord
import UCap.Replica.Debug
import UCap.Replica.MRep

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

sendMsg
  :: (HttpCS g)
  => Client.Manager
  -> Debug -- ^ Debug
  -> Map RId (String,Int) -- ^ Addresses
  -> RId -- ^ Source replica
  -> RId -- ^ Destination replica
  -> BMsg' g
  -> IO ()
sendMsg man debug addrs src dst msg = do
  addr <- case Map.lookup dst addrs of
            Just (addr,port) -> return $ "http://" ++ addr ++ ":" ++ show port ++ "/"
            Nothing -> error $ show dst ++ " has no network address"
  initialRequest <- Client.parseRequest addr
  let req = initialRequest 
              { Client.method = "POST"
              , Client.requestBody =
                  Client.RequestBodyLBS $ encode (src,msg)
              }
      dbl = case msg of
              BPing _ _ -> 3
              BPong _ _ -> 3
              BCoord _ _ -> 2
              _ -> 1
  debug DbTransport dbl $ "SEND(" ++ dst ++ ") " ++ show msg
  -- case msg of
  --   BPing _ _ -> return ()
  --   BPong _ _ -> return ()
  --   _ -> debug $ "SEND(" ++ dst ++ ") " ++ show msg
  Exception.catch (Client.httpLbs req man >> return ()) $ \e ->
    case e of
      Client.HttpExceptionRequest _ (Client.ConnectionFailure _) -> 
        debug DbTransport 1 $ "failed send to " ++ show dst
                              ++ ", msg " ++ show msg
      Client.HttpExceptionRequest _ (Client.InternalException _) ->
        debug DbTransport 1 $ "internal exception on send to "
                              ++ show dst ++ ", msg " ++ show msg
      e -> error $ "unhandled http-client exception: " ++ show e
  return ()

mkListener
  :: (HttpCS g)
  => Int
  -> TChan (TBM' g)
  -> Debug
  -> IO ()
mkListener port chan debug = do
  runSettings (setPort port $ defaultSettings) (msgGetter chan debug)

mkSender
  :: (HttpCS g)
  => Debug
  -> Map RId (String,Int)
  -> IO (RId -> RId -> BMsg' g -> IO ())
mkSender debug addrs = do
  man <- Client.newManager Client.defaultManagerSettings
  return $ sendMsg man debug addrs
