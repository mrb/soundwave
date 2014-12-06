module Soundwave (runServer, parser, router, 
                  logger, responder, emptyEnv, updateDB,
                  replicator, snapshotter,
                  ValueMap, DB) where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Data.Sequence (fromList)
import qualified Data.Map.Strict as M
import Control.Monad.State
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC (pack, unpack)
import Data.Binary.Get
import Data.Word
import Data.Foldable (toList)
import Data.Int
import Data.Maybe
import Text.Regex.TDFA
import Text.ProtocolBuffers.WireMessage (messageGet, messagePut)
import Text.ProtocolBuffers.Basic(utf8, uFromString)
import SoundwaveProtos.Datum
import SoundwaveProtos.Value
import SoundwaveProtos.Request
import SoundwaveProtos.Response
import SoundwaveProtos.Snapshot
import qualified Data.Trie as T
import Database.PureCDB
import System.Directory (doesFileExist)
import Control.Arrow ((&&&)) -- thanks, hlint!
import Control.Concurrent.Async
import Data.UnixTime

import Data.Random
import Data.Random.Source.DevRandom
import Data.Random.Extras

type ValueMap = (M.Map Int32 Int32)
type DB = T.Trie ValueMap
type Cluster = [AddrInfo]
type Env = (DB, Maybe Request, Maybe Response, Maybe Storage, Maybe Cluster)
type HandlerFunc = (BL.ByteString, Socket, SockAddr) -> StateT Env IO ()
type Storage = String

data RequestType = Update | Query | Aggregate | PeerContact
data Node = Node SockAddr Socket

randomNode :: Cluster -> IO AddrInfo
randomNode c = do
  n <- runRVar (choice c) DevRandom
  return n

initServer :: String
         -> [HandlerFunc]
         -> String
         -> Maybe Cluster
         -> StateT Env IO ()
initServer port handlerfuncs file peers = do
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

initPersistence :: FilePath -> StateT Env IO ()
initPersistence file = do
   (db, req, resp, storage, cluster) <- get
   fileExists <- lift $ doesFileExist file
   if fileExists then 
     do
       lift $ putStrLn ("Loading existing db: " ++ file)
       r <- liftIO $ openCDB file
       bs <- liftIO $ getBS r (BC.pack "dbstate")
       snapshot <- liftIO $ parseSnapshotBytes (B.concat bs)
       let newDb = snapshotToDB snapshot
       lift $ putStrLn ("Loaded " ++ show (length (toList (dat snapshot))) ++ " keys.")
       put (newDb, req, resp, Just file, cluster)
    else
      do
        makeCDB (addBS (BC.pack "dbstate") (BL.toStrict (messagePut Snapshot {
            dat = fromList (map (uncurry makeDatum) (T.toList db))
          }))) file
        put (db, req, resp, Just file, cluster)
   return ()

initPeers :: Maybe Cluster -> StateT Env IO ()
initPeers peers = do
  (db, req, resp, storage, cluster) <- get
  n <- liftIO $ randomNode (fromJust peers)

  socket <- connectSocket n

  put (db, req, resp, storage, peers)
  return ()

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

parseSnapshotBytes :: B.ByteString -> IO Snapshot
parseSnapshotBytes s = case messageGet (BL.fromStrict s) of
                Right (snapshot, x) | BL.length x == 0 ->
                  return snapshot
                Right (snapshot, x) | BL.length x /= 0 ->
                  error "Failed to parse snapshot"
                Left error_message ->
                  error $ "Failed to parse snapshot" ++ error_message

snapshotToDB :: Snapshot -> DB
snapshotToDB s = do
    let tupleize = map (name &&& vector) (toList (dat s))
    let namestransform = map (\(x,y) -> (BL.toStrict (utf8 x),y)) tupleize
    let finaltuple = map (\(x,y) -> (x, M.fromList (map (key &&& value) (toList y)))) namestransform
    T.fromList finaltuple

parseRequestBytes :: B.ByteString -> IO Request
parseRequestBytes s = case messageGet (BL.fromStrict s) of
                Right (request, x) | BL.length x == 0 ->
                  return request
                Right (request, x) | BL.length x /= 0 ->
                  error "Failed to parse request"
                Left error_message ->
                  error $ "Failed to parse request" ++ error_message

makeDatum :: B.ByteString -> M.Map Int32 Int32 -> Datum
makeDatum k v = Datum { 
  name = uFromString (BC.unpack k),
  vector = fromList (map (uncurry Value) (M.toList v))
}

makeResponse :: DB -> Response
makeResponse db =  Response { 
  response = fromList (map (uncurry makeDatum) (T.toList db))
}

parser :: HandlerFunc
parser (msg, _, _) = do
  (db, req, resp, storage, cluster) <- get
  let (len, bytes) = runGet readFramedMessage msg
  parsedRequest <- lift $ parseRequestBytes bytes
  put (db, Just parsedRequest, resp, storage, cluster)

router :: HandlerFunc
router _ = do
  (db, req, resp, storage, cluster) <- get
  let datum = request (fromJust req)
  let n = utf8 (name datum)
  let m = M.fromList (map (\x -> (fromIntegral (key x), fromIntegral (value x)))
                          (toList (vector datum)))

  case requestType req of
    Query -> queryData (BL.toStrict n)
    Aggregate -> aggregateData (BL.toStrict n)
    Update -> updateData n m
    PeerContact -> peerContact n

  return ()

requestType :: Maybe Request -> RequestType
requestType r = do
  let datum = request (fromJust r)
  let n = utf8 (name datum)
  
  if n =~ "%" :: Bool then
    Query
  else if n =~ "\\*" :: Bool then
    Aggregate
  else if n =~ "^n:" :: Bool then
    PeerContact
  else
    Update

queryData :: B.ByteString -> StateT Env IO ()
queryData n = do
  (db, req, resp, storage, cluster) <- get
  let (b,_,_) = (n =~ "%") :: (B.ByteString, B.ByteString, B.ByteString)
  let matchedDb = T.submap b db
  if T.null matchedDb then
    put (db, req, Nothing, storage, cluster)
  else
    do
      let newResp = makeResponse matchedDb
      put (db, req, Just newResp, storage, cluster)

aggregateData :: B.ByteString -> StateT Env IO ()
aggregateData n = do
  (db, req, resp, storage, cluster) <- get
  let (b,_,_) = (n =~ "\\*") :: (B.ByteString, B.ByteString, B.ByteString)
  let matchedDb = T.submap b db
  if T.null matchedDb then
    put (db, req, Nothing, storage, cluster)
  else
    do
      let dbi = T.insert n (M.unionsWith max (map snd (T.toList matchedDb))) T.empty
      lift $ print dbi
      let newResp = makeResponse dbi
      put (db, req, Just newResp, storage, cluster)

updateData :: BL.ByteString -> ValueMap -> StateT Env IO ()
updateData n m  = do
  (db, req, resp, storage, cluster) <- get
  respondAndPut (BL.toStrict n) (req, updateDB n m db, resp, storage, cluster)
  where
    respondAndPut key (req, newDb, resp, storage, cluster) =
      if T.null newDb then
        put (newDb, req, resp, storage, cluster)
      else
        do
          let r = T.lookup key newDb
          let respDb = T.insert key (fromJust r) T.empty
          let newResp = makeResponse respDb
          put (newDb, req, Just newResp, storage, cluster)

peerContact :: BL.ByteString -> StateT Env IO ()
peerContact n = do
  (db, req, resp, storage, cluster) <- get
  lift $ print "-----------!!!!"
  put (db, req, Nothing, storage, cluster)

updateDB :: BL.ByteString -> ValueMap -> DB -> DB
updateDB k v db = if T.member (BL.toStrict k) db then
    T.insert (BL.toStrict k) (M.unionWith max v (fromJust $ T.lookup (BL.toStrict k) db)) db
  else
    T.insert (BL.toStrict k) v db
 
logger :: HandlerFunc
logger _ = do
    (db, req, resp, storage, cluster) <- get
    time <- lift $ getUnixTime
    lift $ putStrLn (show (utSeconds time))
    case requestType req of
      Query -> lift $ putStrLn ("[REQ] " ++ show req)
      Update -> lift $ putStrLn ("[REQ] " ++ show req ++ " [DB] " ++ show db)
      Aggregate -> lift $ putStrLn ("[REQ] " ++ show req)
      PeerContact -> lift $ putStrLn ("[REQ] " ++ show req)
    lift $ putStrLn (" [RESP] " ++ show resp)
    return ()

snapshotter :: HandlerFunc
snapshotter _ = do
  (db, req, resp, storage, cluster) <- get

  case requestType req of
    Query -> liftIO $ return ()
    Aggregate -> liftIO $ return ()
    Update -> makeCDB (addBS (BC.pack "dbstate") (BL.toStrict (messagePut Snapshot {
            dat = fromList (map (uncurry makeDatum) (T.toList db))
          }))) (fromJust storage)
    PeerContact -> liftIO $ return ()
  return ()

replicator :: HandlerFunc
replicator _ = do
  (_, _, _, _, cluster) <- get
  lift $ putStrLn (show cluster)
  return ()

responder :: HandlerFunc
responder (_, sock, addr) = do
  (db, req, resp, storage, cluster) <- get
  case resp of
    Just r -> liftIO $ sendTo sock (BL.toStrict (messagePut r)) addr
    Nothing -> liftIO $ sendTo sock B.empty addr
  return ()

emptyEnv :: Env
emptyEnv = (T.empty, Nothing, Nothing, Nothing, Nothing)
 
runServer :: String -> [(String, String)] -> String -> [HandlerFunc] -> IO ()
runServer port peers file handlerfuncs  = do
  cluster <- mapM (\(hostname, port) -> do
    addr <- (getAddrInfo Nothing (Just hostname) (Just port))
    return (head addr)) peers
  void $ withSocketsDo $ runStateT (initServer port handlerfuncs file (Just cluster)) emptyEnv
