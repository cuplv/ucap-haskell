{-# LANGUAGE DeriveGeneric #-}

module Data.UCap.Eff
  ( Eff
  , eff
  , cFun'
  ) where

import Data.UCap.Classes

import Data.Aeson
import Data.Maybe (fromJust)
import GHC.Generics

newtype Eff c = Eff c deriving (Show,Eq,Ord,Generic)

instance (ToJSON n) => ToJSON (Eff n) where
  toEncoding = genericToEncoding defaultOptions
instance (FromJSON n) => FromJSON (Eff n)

eff :: (Cap c) => c -> Maybe (Eff c)
eff c = case maxeff c of
          Just _ -> Just $ Eff c
          Nothing -> Nothing

cFun' :: (Cap c) => Eff c -> State' c -> State' c
cFun' (Eff c) = eFun . fromJust . maxeff $ c
