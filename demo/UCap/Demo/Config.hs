{-# LANGUAGE OverloadedStrings #-}

module UCap.Demo.Config
  ( GlobalConfig (..)
  , dGlobalConfig
  , LocalConfig (..)
  , dLocalConfig
  , SimpleEx (..)
  , dSimpleEx
  , dhallInput
  ) where

import UCap.Coord
import UCap.Domain
import UCap.Replica.HttpDemo
import UCap.Replica.MRep (Addrs,RId)

import Data.Text (pack)
import Data.Time.Clock
import Dhall

-- data Config
--   = Config { exprSettings :: ExConf
--            , localId :: String
--            , repRole :: Role
--            , debugLvl :: Int
--            , exprType :: ExperimentType
--            , network :: Addrs
--            }

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
  | EscrowEx { initOwner :: String, amount :: Int }

dSimpleEx :: Decoder SimpleEx
dSimpleEx = union $
  (TokenEx
     <$> constructor "Token" (record $ field "initOwner" string))

type LG = TokenG String IntC

type EG = IntEscrow String

dhallInput :: Decoder a -> String -> IO a
dhallInput d = input d . pack

-- inputConfig :: FilePath -> IO Config
-- inputConfig = input dConfig . pack

-- dConfig :: Decoder Config
-- dConfig = record $ Config
--   <$> field "network" dCommon
--   <*> field "experiment" dExConf
--   <*> field "id" string
--   <*> field "role" string
--   <*> (fromIntegral <$> field "debug" natural)
--   <*> field "exprType" _
--   <*> field "network"

-- dExConf :: Decoder ExConf
-- dExConf = record $ ExConf
--   <$> (fromIntegral <$> field "rate" natural)
--   <*> (secondsToNominalDiffTime . fromIntegral
--        <$> field "duration" natural)

-- dCommon :: Decoder (HRSettings LG)
-- dCommon = record $ HRSettings
--   <$> field "addresses" (Dhall.map string dAddr)
--   <*> field "initState" int
--   <*> (mkTokenG <$> field "initOwner" string)

dAddr :: Decoder (String,Int)
dAddr = record $ (,)
  <$> field "host" string
  <*> (fromIntegral <$> field "port" natural)
