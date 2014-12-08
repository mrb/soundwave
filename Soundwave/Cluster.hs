module Soundwave.Cluster where

import Soundwave.Data
import qualified Data.ByteString.Lazy as BL
import Control.Concurrent.Async
import Control.Arrow ((&&&)) -- thanks, hlint!
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC (pack, unpack)
import qualified Data.Map.Strict as M
import Data.Foldable (toList)
import Data.Binary.Get
import Data.Binary.Put
import Data.Maybe
import Text.Regex.TDFA
import Text.ProtocolBuffers.WireMessage (messageGet, messagePut)
import Text.ProtocolBuffers.Basic(utf8, uFromString, toUtf8)
import SoundwaveProtos.Datum
import SoundwaveProtos.Value
import SoundwaveProtos.Request
import SoundwaveProtos.Response
import SoundwaveProtos.Snapshot

import Data.Random
import Data.Random.Source.DevRandom
import Data.Random.Extras

initPeers :: Maybe Cluster -> StateT Env IO ()
initPeers peers = do
  env <- get
  put env { cluster = peers }

peerContact :: B.ByteString -> StateT Env IO ()
peerContact n = do
  env <- get
  let (_,_,d) = (n =~ "^n:") :: (B.ByteString, B.ByteString, B.ByteString)
  let vmap = M.fromList (map (key &&& value) (toList (vector (request (fromJust (req env))))))
  let newDB = updateDB (BL.fromStrict d) vmap (db env)

  put env { resp = Nothing, db = newDB }

replicator :: HandlerFunc
replicator _ = do
  env <- get

  case requestType (req env) of
    Query -> liftIO $ return ()
    Aggregate -> liftIO $ return ()
    Update -> do
      node <- liftIO $ randomNode (fromJust (cluster env))
      sock <- lift $ socket (addrFamily node) Datagram defaultProtocol
    
      let r = req env
      let d = request (fromJust r)
      let newName = BC.unpack (B.concat [(BC.pack "n:"), (BL.toStrict (utf8 (name d)))])
      let newReq = Request { request = Datum { name = (uFromString newName), vector = (vector d)}}
    
      let packedRequest = (BL.toStrict (messagePut newReq))
      let framedRequest = runPut (frameMessage (fromIntegral (B.length packedRequest)) packedRequest)
      liftIO $ sendTo sock (BL.toStrict framedRequest) (addrAddress node)
      return ()
    PeerContact -> liftIO $ return ()
  
  return ()

randomNode :: Cluster -> IO AddrInfo
randomNode c = do
  n <- runRVar (choice c) DevRandom
  return n

