module Soundwave.Handlers where

import qualified Soundwave.Logger as L
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Control.Monad.State
import qualified Data.Map.Strict as M
import Data.Int
import Data.Maybe
import Data.Word
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.Trie as T
import Data.Sequence (fromList)
import Data.Foldable (toList)
import Text.ProtocolBuffers.WireMessage (messageGet, messagePut)
import Text.ProtocolBuffers.Basic(utf8, uFromString, toUtf8)
import SoundwaveProtos.Datum
import SoundwaveProtos.Value
import SoundwaveProtos.Request
import SoundwaveProtos.Response
import SoundwaveProtos.Snapshot
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC (pack, unpack)
import Text.Regex.TDFA

import Soundwave.Data
import Soundwave.Cluster
import Soundwave.Persistence
import Soundwave.Query

parser :: HandlerFunc
parser (msg, _, _) = do
  env <- get
  let (len, bytes) = runGet readFramedMessage msg
  parsedRequest <- lift $ parseRequestBytes bytes
  put env { req = Just parsedRequest }

router :: HandlerFunc
router _ = do
  env <- get
  let datum = request (fromJust (req env))
  let n = utf8 (name datum)
  let m = M.fromList (map (\x -> (fromIntegral (key x), fromIntegral (value x)))
                          (toList (vector datum)))

  lift $ L.info (logger env) ((show (requestType (req env))) ++ " " ++ (show (req env)))

  case requestType (req env) of
    Query -> queryData (BL.toStrict n)
    Aggregate -> aggregateData (BL.toStrict n)
    Update -> updateData n m
    PeerContact -> peerContact (BL.toStrict n)

  return ()

snapshotter :: HandlerFunc
snapshotter _ = do
  env <- get

  case requestType (req env) of
    Query -> liftIO $ return ()
    Aggregate -> liftIO $ return ()
    Update -> liftIO $ saveSnapshot (db env) (fromJust (storage env)) 
    PeerContact -> liftIO $ return ()
  return ()

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

responder :: HandlerFunc
responder (_, sock, addr) = do
  env <- get
  case (resp env) of
    Just r -> liftIO $ sendTo sock (BL.toStrict (messagePut r)) addr
    Nothing -> liftIO $ sendTo sock B.empty addr
  return ()
