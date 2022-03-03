{-# LANGUAGE OverloadedStrings #-}

module UCap.Demo.Config
  ( LocalConfig (..)
  , dLocalConfig
  , Experiment (..)
  , dExperiment
  , SimpleEx (..)
  , dSimpleEx
  , CombinedConfig
  , dCombinedConfig
  , dhallInput
  ) where

import UCap.Coord
import UCap.Domain
import UCap.Replica.Debug
import UCap.Replica.HttpDemo
import UCap.Replica.MRep (Addrs,RId)

import qualified Data.Map as Map
import Data.Text (pack)
import Data.Time.Clock
import Dhall

type CombinedConfig e = (Addrs, LocalConfig, [Experiment e])

dCombinedConfig d = record $ (,,)
  <$> field "network" (Dhall.map string dAddr)
  <*> field "local" dLocalConfig
  <*> field "experiments" (list $ dExperiment d)


data Experiment e
  = Experiment { exConf :: ExConf, exSetup :: e }

dExperiment :: Decoder e -> Decoder (Experiment e)
dExperiment d = record $ Experiment
  <$> (ExConf
         <$> field "rate" double
         <*> (secondsToNominalDiffTime . fromIntegral
              <$> field "duration" natural))
  <*> field "setup" d

data LocalConfig
  = LocalConfig { lcId :: RId
                , lcDebug :: DebugConf
                , lcOutPath :: Maybe FilePath
                }

dLocalConfig :: Decoder LocalConfig
dLocalConfig = record $ LocalConfig
  <$> field "id" string
  <*> field "debug" dDebugConf
  <*> field "outPath" (Dhall.maybe string)

data SimpleEx
  = TokenEx
  | EscrowEx { amount :: Int, bfactor :: Int }

instance Show SimpleEx where
  show TokenEx = "token"
  show (EscrowEx n b) = "escrow-" ++ show b

dSimpleEx :: Decoder SimpleEx
dSimpleEx = union $
  (const TokenEx <$> constructor "Token" unit)
  <> (constructor "Escrow" . record $ EscrowEx
        <$> (fromIntegral <$> field "amount" natural)
        <*> (fromIntegral <$> field "bufferFactor" natural))

type LG = TokenG String IntC

type EG = IntEscrow String

dhallInput :: Decoder a -> String -> IO a
dhallInput d = input d . pack

dAddr :: Decoder (String,Int)
dAddr = record $ (,)
  <$> field "host" string
  <*> (fromIntegral <$> field "port" natural)

intNat :: Decoder Int
intNat = fromIntegral <$> natural

dDebugConf :: Decoder DebugConf
dDebugConf = record $ 
  (Map.insert DbTransport <$> field "transport" intNat)
  <*> ((Map.insert DbMainLoop <$> field "mainLoop" intNat)
        <*> ((Map.insert DbScript <$> field "script" intNat)
              <*> (Map.insert DbSetup <$> field "setup" intNat
                   <*> pure Map.empty)))
