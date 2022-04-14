{-# LANGUAGE FlexibleContexts #-}

module UCap.Replica.Transact
  ( transact
  , grantRequests'
  , acceptGrants'
  ) where

import UCap.Coord
import UCap.Domain
import UCap.Lens
import UCap.Op
import UCap.Replica.Script

import Control.Monad.Except
import Data.Maybe (fromJust)

{-| Check if the replica has the given 'Caps'.  If so, return a state
  value that is a safe read under those capabilities.  If not, return
  an updated 'CoordSys' in which those capabilities have been
  requested. -}
execCaps
  :: (CoordSys g, Show (GCap g), Show g, Show (GId g))
  => GId g
  -> g
  -> GState g
  -> Caps (GCap g)
  -> Either g (GState g)
execCaps rid g s cs =
  case resolveCaps rid cs g of
    Right sim -> Right $ eFun sim s
    Left (Just g') -> Left g'
    Left Nothing -> error $
      "No way for " ++ show rid
      ++ "to request the needed caps " ++ show cs
      ++ "from coord " ++ show g

runUpdate
  :: (CoordSys g, Monad m)
  => Op (GCap g) (ExceptT () m) () a
  -> GState g
  -> ScriptT g m (Maybe a)
runUpdate t s = do
  rid <- getReplicaId
  rslt <- liftScript . runExceptT . fromJust $ execWith fullCaps s t
  case rslt of
    Right (_,e,a) -> do
      g <- view rsCoord <$> readState
      let g' = case resolveEffect rid e g of
                 Right g' -> g'
                 Left _ -> error "Write error."
      writeGE g' e
      return $ Just a
    Left () -> return Nothing

transact
  :: (CoordSys g, Monad m, Show (GCap g), Show g, Show (GId g))
  => Op (GCap g) (ExceptT () m) () a
  -> ScriptT g m (Block' (ScriptT g m) (Maybe a))
transact t = do
  rid <- getReplicaId
  ctx <- readState
  let r = execCaps rid (ctx^.rsCoord) (ctx^.rsStore) (capsReq t)
  case r of
    Right s -> do
      a <- runUpdate t s
      return . nonBlock $ (return a)
    Left g' -> do
      setCoord g'
      return $ do
        ctx <- checkState
        let r = execCaps rid (ctx^.rsCoord) (ctx ^.rsStore) (capsReq t)
        case r of
          Right s -> nonBlock $ runUpdate t s
          Left _ -> notReady

grantRequests' :: (CoordSys g, Monad m) => Block' (ScriptT g m) ()
grantRequests' = do
  rid <- lift getReplicaId
  g <- view rsCoord <$> checkState
  case grantRequests rid g of
    Just _ -> nonBlock $ do
      g' <- fromJust . grantRequests rid . view rsCoord <$> readState
      setCoord g'
    Nothing -> notReady

acceptGrants' :: (CoordSys g, Monad m) => Block' (ScriptT g m) ()
acceptGrants' = do
  rid <- lift getReplicaId
  g <- view rsCoord <$> checkState
  case acceptGrants rid g of
    Just g' -> nonBlock $ setCoord g'
    Nothing -> notReady
