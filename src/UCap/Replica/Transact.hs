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
  :: (CoordSys g)
  => GId g
  -> g
  -> GState g
  -> Caps (GCap g)
  -> Either g (GState g)
execCaps rid g s cs =
  case resolveCaps rid cs g of
    Right sim -> Right $ eFun sim s
    Left (Just g') -> Left g'
    Left Nothing -> error "No way to request the needed caps."

-- {-| Compile an operation into a replica script.  If capabilities are not
--   sufficient, @'Left' g@ is returned, where @g@ is an updated
--   coordination system in which requests for the capabilities have been
--   made. -}
-- transactSimple
--   :: (CoordSys g, Monad m)
--   => Op (GCap g) m () a
--   -> ScriptT g m (Either g a)
-- transactSimple t = do
--   let caps = capsReq t
--   rid <- getReplicaId
--   ctx <- readState
--   case resolveCaps rid caps (ctx^.rsCoord) of
--     Right sim -> do
--       -- We've already checked the capabilities, so we feed 'fullCaps'
--       -- to execWith.  I should define a version of execWith that
--       -- doesn't bother with capabilities, since the CoordSys does all
--       -- the reasoning about them.
--       let s = eFun sim $ ctx^.rsStore
--       (_,e,a) <- liftScript . fromJust $ execWith fullCaps s t
--       let g' = case resolveEffect rid e (ctx^.rsCoord) of
--                  Right g' -> g'
--                  Left _ -> error "Write error."
--       -- emitEffect e
--       -- setCoord g'
--       writeGE g' e
--       return (Right a)
--     Left (Just g') -> return (Left g')
--     Left Nothing -> error "No way to request the needed caps."

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
  -- g <- view rsCoord <$> readState
  -- let g' = case resolveEffect rid e g of
  --            Right g' -> g'
  --            Left _ -> error "Write error."
  -- -- emitEffect e
  -- -- setCoord g'
  -- writeGE g' e
  -- return a

transact
  :: (CoordSys g, Monad m)
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

-- transactMany
--   :: (CoordSys g, Monad m)
--   => [Op (GCap g) m () a]
--   -> ScriptT g m [a]
-- transactMany [] = return []
-- transactMany (o1:os) = do
--   complete1 <- transact o1
--   await . firstOf $
--     [ grantRequests' `andThen_` transactMany (o1:os)
--     , acceptGrants' `andThen_` transactMany (o1:os)
--     , complete1 `andThen` (\a -> (a :) <$> transactMany os)
--     ]

-- transactMany_
--   :: (CoordSys g, Monad m)
--   => [Op (GCap g) m () a]
--   -> ScriptT g m ()
-- transactMany_ [] = return ()
-- transactMany_ (o1:os) = do
--   complete1 <- transact o1
--   await . firstOf $
--     [ grantRequests' `andThen_` transactMany_ (o1:os)
--     , acceptGrants' `andThen_` transactMany_ (o1:os)
--     , complete1 `andThen_` transactMany_ os
--     ]

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
