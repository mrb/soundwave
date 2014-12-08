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
import Text.ProtocolBuffers.WireMessage (messageGet, messagePut)
import Text.ProtocolBuffers.Basic(utf8, uFromString, toUtf8)
import SoundwaveProtos.Datum
import SoundwaveProtos.Value
import SoundwaveProtos.Request
import qualified Data.Trie as T
import Data.UnixTime

import Soundwave.Data
import Soundwave.Cluster
import qualified Soundwave.Logger as L
import Soundwave.Persistence

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

makeAddr :: String -> String -> StateT Env IO AddrInfo
makeAddr host port = do
  let config = (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
  addrinfos <- lift $ getAddrInfo config (Just host) (Just port)
  let serveraddr = head addrinfos
  return serveraddr

connectSocket :: AddrInfo -> StateT Env IO Socket
connectSocket addr = do
  sock <- lift $ socket (addrFamily addr) Datagram defaultProtocol
  lift $ bindSocket sock (addrAddress addr)
  return sock


processSocket :: Socket ->
                 [HandlerFunc] ->
                 StateT Env IO ()
processSocket sock handlerfuncs = do
  (msg, addr) <- lift $ recvFrom sock 4096
  do
    mapM_ (\h -> h (BL.fromStrict msg, sock, addr)) handlerfuncs
    processSocket sock handlerfuncs
  return ()

readFramedMessage :: Get (Word32, B.ByteString)
readFramedMessage = do
  len <- getWord32be
  msg <- getByteString (fromIntegral len)
  return (len, msg)

parseRequestBytes :: B.ByteString -> IO Request
parseRequestBytes s = case messageGet (BL.fromStrict s) of
                Right (request, x) | BL.length x == 0 ->
                  return request
                Right (request, x) | BL.length x /= 0 ->
                  error "Failed to parse request"
                Left error_message ->
                  error $ "Failed to parse request" ++ error_message

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

queryData :: B.ByteString -> StateT Env IO ()
queryData n = do
  env <- get
  let (b,_,_) = (n =~ "%") :: (B.ByteString, B.ByteString, B.ByteString)
  let matchedDb = T.submap b (db env)
  if T.null matchedDb then
    put env { resp = Nothing }
  else
    do
      let newResp = makeResponse matchedDb
      put env { resp = Just newResp }

aggregateData :: B.ByteString -> StateT Env IO ()
aggregateData n = do
  env <- get
  let (b,_,_) = (n =~ "\\*") :: (B.ByteString, B.ByteString, B.ByteString)
  let matchedDb = T.submap b (db env)
  if T.null matchedDb then
    put env { resp = Nothing }
  else
    do
      let dbi = T.insert n (M.unionsWith max (map snd (T.toList matchedDb))) T.empty
      let newResp = makeResponse dbi
      put env { resp = Just newResp }

updateData :: BL.ByteString -> ValueMap -> StateT Env IO ()
updateData n m  = do
  env <- get
  let ndb = updateDB n m (db env)
  respondAndPut (BL.toStrict n) env { db = ndb }
  where
    respondAndPut key e =
      if T.null (db e) then
        put e
      else
        do
          let r = T.lookup key (db e)
          let respDb = T.insert key (fromJust r) T.empty
          let newResp = makeResponse respDb
          put e { resp = Just newResp }

responder :: HandlerFunc
responder (_, sock, addr) = do
  env <- get
  case (resp env) of
    Just r -> liftIO $ sendTo sock (BL.toStrict (messagePut r)) addr
    Nothing -> liftIO $ sendTo sock B.empty addr
  return ()

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
