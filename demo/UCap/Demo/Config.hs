{-# LANGUAGE OverloadedStrings #-}

module UCap.Demo.Config
  ( GlobalConfig (..)
  , dGlobalConfig
  , LocalConfig (..)
  , dLocalConfig
  , SimpleEx (..)
  , dSimpleEx
  , CombinedConfig
  , dCombinedConfig
  , dhallInput
  ) where

import UCap.Coord
import UCap.Domain
import UCap.Replica.HttpDemo
import UCap.Replica.MRep (Addrs,RId)

import Data.Text (pack)
import Data.Time.Clock
import Dhall

type CombinedConfig e = (GlobalConfig e, LocalConfig)

dCombinedConfig d = record $ (,)
  <$> field "global" (dGlobalConfig d)
  <*> field "local" dLocalConfig

data GlobalConfig e
  = GlobalConfig { gcNetwork :: Addrs
                 , gcExConf :: ExConf
                 , gcExSetup :: e                 
                 }

dGlobalConfig :: Decoder e -> Decoder (GlobalConfig e)
dGlobalConfig d = record $ GlobalConfig
  <$> field "network" (Dhall.map string dAddr)
  <*> (ExConf
         <$> field "rate" double
         <*> (secondsToNominalDiffTime . fromIntegral
              <$> field "duration" natural))
  <*> field "setup" d

data LocalConfig
  = LocalConfig { lcId :: RId
                , lcDebug :: Int
                }

dLocalConfig :: Decoder LocalConfig
dLocalConfig = record $ LocalConfig
  <$> field "id" string
  <*> (fromIntegral <$> field "debug" natural) 

data SimpleEx
  = TokenEx { initOwner :: String }
  | EscrowEx { initOwner :: String, amount :: Int, bfactor :: Int }

dSimpleEx :: Decoder SimpleEx
dSimpleEx = union $
  (TokenEx
     <$> constructor "Token" (record $ field "initOwner" string))
  <> (constructor "Escrow" . record $ EscrowEx
        <$> field "initOwner" string
        <*> (fromIntegral <$> field "amount" natural)
        <*> (fromIntegral <$> field "bufferFactor" natural))

type LG = TokenG String IntC

type EG = IntEscrow String

dhallInput :: Decoder a -> String -> IO a
dhallInput d = input d . pack

dAddr :: Decoder (String,Int)
dAddr = record $ (,)
  <$> field "host" string
  <*> (fromIntegral <$> field "port" natural)
