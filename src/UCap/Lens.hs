{-# LANGUAGE RankNTypes #-}

module UCap.Lens
  ( module Lens.Micro.Platform
  , (/\~)
  , meetTo
  , plusTo
  ) where

import UCap.Domain.Classes (Meet (..), BMeet (..))

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Lens.Micro.Platform

(/\~) :: (Meet a) => ASetter s t a a -> a -> s -> t
(/\~) l b = l %~ meet b

meetTo :: (Meet a, BMeet s) => ASetter s t a a -> a -> t
meetTo l b = meetId & l /\~ b

plusTo :: (Monoid a, Monoid s) => ASetter s t a a -> a -> t
plusTo l b = mempty & l <>~ b

{-| Like 'Lens.Micro.Platform.non', but with an explicit check for
  whether a value is the empty value.  This is useful for types
  that do not implement 'Eq', but nonetheless have a clearly
  identifyable "empty" value. -}
nani :: a -> (a -> Bool) -> Lens' (Maybe a) a
nani x pr afb s = f <$> afb (fromMaybe x s)
  where f y = if pr y
                 then Nothing
                 else Just y

{-| A version of 'nani' using 'Data.Monoid.mempty' as the empty value
  and 'Data.Foldable.null' as the empty value test. -}
nonNull :: (Foldable t, Monoid (t a)) => Lens' (Maybe (t a)) (t a)
nonNull = nani mempty null

{-| 'nonNull' specialized to lists, with @[]@ as the empty value. -}
nonList :: Lens' (Maybe [a]) [a]
nonList = nonNull

{-| 'nonNull' specialized to maps, with @Data.Map.empty@ as the empty
  value. -}
nonMap :: (Ord k) => Lens' (Maybe (Map k a)) (Map k a)
nonMap = nonNull
