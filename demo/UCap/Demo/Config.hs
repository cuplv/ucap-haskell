{-# LANGUAGE OverloadedStrings #-}

module UCap.Demo.Config
  ( Config (..)
  , inputConfig
  ) where

import UCap.Coord
import UCap.Domain
import UCap.Replica.HttpDemo

import Data.Text (pack)
import Data.Time.Clock
import Dhall

data Config
  = Config { commonSettings :: HRSettings LG
           , exprSettings :: ExConf
           , localId :: String
           , repRole :: String
           }

type LG = TokenG String IntC

type EG = IntEscrow String

inputConfig :: FilePath -> IO Config
inputConfig = input dConfig . pack

dConfig :: Decoder Config
dConfig = record $ Config
  <$> field "network" dCommon
  <*> field "experiment" dExConf
  <*> field "id" string
  <*> field "role" string

dExConf :: Decoder ExConf
dExConf = record $ ExConf
  <$> (fromIntegral <$> field "rate" natural)
  <*> (secondsToNominalDiffTime . fromIntegral
       <$> field "duration" natural)

dCommon :: Decoder (HRSettings LG)
dCommon = record $ HRSettings
  <$> field "addresses" (Dhall.map string dAddr)
  <*> field "initState" int
  <*> (mkTokenG <$> field "initOwner" string)

dAddr :: Decoder (String,Int)
dAddr = record $ (,)
  <$> field "host" string
  <*> (fromIntegral <$> field "port" natural)
