{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module UCap.Replica.Http where

import UCap.Coord
import UCap.Replica.MRep

import Control.Concurrent.STM
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
  ) => HttpCS g
instance
  ( MCS g
  , ToJSON g
  , ToJSON (GCap g)
  , ToJSON (GEffect g)
  , FromJSON g
  , FromJSON (GCap g)
  , FromJSON (GEffect g)
  ) => HttpCS g

msgGetter
  :: (HttpCS g)
  => TChan (TBM' g)
  -> Application
msgGetter chan request respond = do
  body <- strictRequestBody request
  case decode body of
    Just tbm -> do
      atomically (writeTChan chan tbm)
      respond $ responseLBS status200 [] ""
    Nothing -> respond $ responseLBS status400 [] ""

sendMsg
  :: (HttpCS g)
  => Client.Manager
  -> Map RId (String,Int) -- ^ Addresses
  -> RId -- ^ Source replica
  -> RId -- ^ Destination replica
  -> BMsg' g
  -> IO ()
sendMsg man addrs src dst msg = do
  addr <- case Map.lookup dst addrs of
            Just (addr,port) -> return $ addr ++ ":" ++ show port
            Nothing -> error $ show dst ++ " has no network address"
  initialRequest <- Client.parseRequest addr
  let req = initialRequest 
              { Client.method = "POST"
              , Client.requestBody =
                  Client.RequestBodyLBS $ encode (src,msg)
              }
  response <- Client.httpLbs req man
  return ()

mkListener :: (HttpCS g) => Int -> TChan (TBM' g) -> IO ()
mkListener port chan = do
  runSettings (setPort port $ defaultSettings) (msgGetter chan)
