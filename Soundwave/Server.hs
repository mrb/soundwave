module Soundwave.Server (runServer, parser, router, 
                  responder, emptyEnv, updateDB,
                  replicator, snapshotter,
                  ValueMap, DB, Cluster) where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.Map.Strict as M
import Control.Monad.State
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import Data.Foldable (toList)
import Data.Int
import Data.Maybe
import Text.Regex.TDFA
import SoundwaveProtos.Datum
import SoundwaveProtos.Value
import SoundwaveProtos.Request
import qualified Data.Trie as T
import Data.UnixTime

import Soundwave.Data
import Soundwave.Cluster
import qualified Soundwave.Logger as L
import Soundwave.Persistence
import Soundwave.Network
import Soundwave.Handlers
import Soundwave.Query

initServer :: String
         -> [HandlerFunc]
         -> String
         -> Maybe Cluster
         -> StateT Env IO ()
initServer port handlerfuncs file peers = do
 env <- get
 addr <- (makeAddr "127.0.0.1" port)
 sock <- connectSocket addr
 initPersistence file
 initPeers peers
 processSocket sock handlerfuncs

emptyEnv :: L.Log -> Env
emptyEnv l = Env { db = T.empty,
                 req = Nothing,
                 resp = Nothing,
                 storage = Nothing,
                 cluster = Nothing,
                 logger = l
               }
 
runServer :: String -> [(String, String)] -> String -> [HandlerFunc] -> L.Log -> IO ()
runServer port peers file handlerfuncs l = do
  cluster <- mapM (\(hostname, port) -> do
    addr <- (getAddrInfo Nothing (Just hostname) (Just port))
    return (head addr)) peers
  void $ withSocketsDo $ runStateT (initServer port handlerfuncs file (Just cluster)) (emptyEnv l)
