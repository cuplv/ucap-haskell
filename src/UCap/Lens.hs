module UCap.Lens
  ( module Lens.Micro.Platform
  , (/\~)
  , meetTo
  , plusTo
  ) where

import UCap.Domain.Classes (Meet (..), BMeet (..))

import Lens.Micro.Platform

(/\~) :: (Meet a) => ASetter s t a a -> a -> s -> t
(/\~) l b = l %~ meet b

meetTo :: (Meet a, BMeet s) => ASetter s t a a -> a -> t
meetTo l b = meetId & l /\~ b

plusTo :: (Monoid a, Monoid s) => ASetter s t a a -> a -> t
plusTo l b = mempty & l <>~ b
