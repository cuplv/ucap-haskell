module UCap.Replica.Debug where

import Data.Map (Map)
import qualified Data.Map as Map

data DebugCategory
  = DbTransport
  | DbMainLoop
  | DbScript
  | DbSetup
  deriving (Eq,Ord)

instance Show DebugCategory where
  show DbTransport = "transport"
  show DbMainLoop = "mainLoop"
  show DbScript = "script"
  show DbSetup = "setup"

type DebugConf = Map DebugCategory Int

dbConf :: DebugConf -> DebugCategory -> Int
dbConf m k = case Map.lookup k m of
               Just n -> n
               Nothing -> 0

anyDebug :: DebugConf -> Bool
anyDebug = or . map (> 0) . Map.elems

mkDebug :: DebugConf -> (DebugCategory -> String -> IO ()) -> Debug
mkDebug m f = \dc dl msg -> if dbConf m dc >= dl
                               then f dc msg
                               else return ()

type Debug = DebugCategory -> Int -> String -> IO ()
