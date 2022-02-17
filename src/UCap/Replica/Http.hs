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
  -> (String -> IO ())
  -> Application
msgGetter chan debug request respond = do
  body <- strictRequestBody request
  case decode body of
    Just tbm -> do
      let src = fst tbm
          msg = snd tbm
      case msg of
        BPing _ -> return ()
        BPong _ -> return ()
        _ -> debug $ "RECV(" ++ src ++ ") " ++ show msg
      atomically (writeTChan chan tbm)
      respond $ responseLBS status200 [] ""
    Nothing -> respond $ responseLBS status400 [] ""

sendMsg
  :: (HttpCS g)
  => Client.Manager
  -> (String -> IO ()) -- ^ Debug
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
  case msg of
    BPing _ -> return ()
    BPong _ -> return ()
    _ -> debug $ "SEND(" ++ dst ++ ") " ++ show msg
  Exception.catch (Client.httpLbs req man >> return ()) $ \e ->
    case e of
      Client.HttpExceptionRequest _ (Client.ConnectionFailure _) -> 
        debug $ "failed send to " ++ show dst ++ ", msg " ++ show msg
      Client.HttpExceptionRequest _ (Client.InternalException _) ->
        debug $ "internal exception on send to "
                ++ show dst ++ ", msg " ++ show msg
      e -> error $ "unhandled http-client exception: " ++ show e
  return ()

mkListener
  :: (HttpCS g)
  => Int
  -> TChan (TBM' g)
  -> (String -> IO ())
  -> IO ()
mkListener port chan debug = do
  runSettings (setPort port $ defaultSettings) (msgGetter chan debug)

mkSender
  :: (HttpCS g)
  => (String -> IO ())
  -> Map RId (String,Int)
  -> IO (RId -> RId -> BMsg' g -> IO ())
mkSender debug addrs = do
  man <- Client.newManager Client.defaultManagerSettings
  return $ sendMsg man debug addrs
