module Soundwave (runServer, requestParser, requestRouter, 
                  logger, responder, emptyEnv, updateDB,
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
import Control.Arrow ((&&&))

type ValueMap = (M.Map Int32 Int32)
type DB = T.Trie ValueMap
type Env = (Maybe Request, DB, Maybe Response, Maybe Storage)
type HandlerFunc = (BL.ByteString, Socket, SockAddr) -> StateT Env IO ()
type Storage = String

data RequestType = Update | Query | Aggregate

initServer :: String
         -> [HandlerFunc]
         -> String
         -> StateT Env IO ()
initServer port handlerfuncs file = do
 addrinfos <- lift $ getAddrInfo
                (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                Nothing (Just port)
 let serveraddr = head addrinfos
 sock <- lift $ socket (addrFamily serveraddr) Datagram defaultProtocol
 lift $ bindSocket sock (addrAddress serveraddr)
 initPersistence file
 processSocket sock handlerfuncs

initPersistence :: FilePath -> StateT Env IO ()
initPersistence file = do
   (req, db, resp, _) <- get
   fileExists <- lift $ doesFileExist file
   if fileExists then 
     do
       lift $ print ("Loading existing db: " ++ file)
       r <- liftIO $ openCDB file
       bs <- liftIO $ getBS r (BC.pack "dbstate")
       snapshot <- liftIO $ parseSnapshotBytes (B.concat bs)
       let newDb = snapshotToDB snapshot
       lift $ print ("Loaded " ++ show (length (toList (dat snapshot))) ++ " keys.")
       put (req, newDb, resp, Just file)
    else
      do
        makeCDB (addBS (BC.pack "dbstate") (BL.toStrict (messagePut Snapshot {
            dat = fromList (map (uncurry makeDatum) (T.toList db))
          }))) file
        put (req, db, resp, Just file)
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

requestParser :: HandlerFunc
requestParser (msg, _, _) = do
  (req, db, resp, storage) <- get
  let (len, bytes) = runGet readFramedMessage msg
  parsedRequest <- lift $ parseRequestBytes bytes
  put (Just parsedRequest, db, resp, storage)

requestRouter :: HandlerFunc
requestRouter _ = do
  (req, db, resp, storage) <- get
  let datum = request (fromJust req)
  let n = utf8 (name datum)
  let m = M.fromList (map (\x -> (fromIntegral (key x), fromIntegral (value x)))
                          (toList (vector datum)))

  case requestType req of
    Query -> queryData (BL.toStrict n)
    Aggregate -> aggregateData (BL.toStrict n)
    Update -> updateData n m

  return ()

requestType :: Maybe Request -> RequestType
requestType r = do
  let datum = request (fromJust r)
  let n = utf8 (name datum)
  
  if n =~ "%" :: Bool then
    Query
  else if n =~ "\\*" :: Bool then
    Aggregate
  else
    Update

queryData :: B.ByteString -> StateT Env IO ()
queryData n = do
  (req, db, resp, storage) <- get
  let (b,_,_) = (n =~ "%") :: (B.ByteString, B.ByteString, B.ByteString)
  let matchedDb = T.submap b db
  if T.null matchedDb then
    put (req, db, Nothing, storage)
  else
    do
      let newResp = makeResponse matchedDb
      put (req, db, Just newResp, storage)

aggregateData :: B.ByteString -> StateT Env IO ()
aggregateData n = do
  (req, db, resp, storage) <- get
  let (b,_,_) = (n =~ "\\*") :: (B.ByteString, B.ByteString, B.ByteString)
  let matchedDb = T.submap b db
  if T.null matchedDb then
    put (req, db, Nothing, storage)
  else
    do
      let newResp = makeResponse $ T.insert n (M.unionsWith max (map snd (T.toList db))) T.empty
      put (req, db, Just newResp, storage)

updateData :: BL.ByteString -> ValueMap -> StateT Env IO ()
updateData n m  = do
  (req, db, resp, storage) <- get
  respondAndPut (BL.toStrict n) (req, updateDB n m db, resp, storage)
  where
    respondAndPut key (req, newDb, resp, storage) =
      if T.null newDb then
        put (req, newDb, resp, storage)
      else
        do
          let r = T.lookup key newDb
          let respDb = T.insert key (fromJust r) T.empty
          let newResp = makeResponse respDb
          put (req, newDb, Just newResp, storage)

updateDB :: BL.ByteString -> ValueMap -> DB -> DB
updateDB k v db = if T.member (BL.toStrict k) db then
    T.insert (BL.toStrict k) (M.unionWith max v (fromJust $ T.lookup (BL.toStrict k) db)) db
  else
    T.insert (BL.toStrict k) v db
 
logger :: HandlerFunc
logger _ = do
    (req, db, resp, storage) <- get

    case requestType req of
      Query -> lift $ print ("[REQ] " ++ show req)
      Update -> lift $ print ("[REQ] " ++ show req ++ " [DB] " ++ show db)
      Aggregate -> lift $ print ("[REQ] " ++ show req)
    
    lift $ print (" [RESP] " ++ show resp)
    
    put (req, db, resp, storage)


snapshotter :: HandlerFunc
snapshotter _ = do
  (req, db, resp, storage) <- get

  case requestType req of
    Update -> makeCDB (addBS (BC.pack "dbstate") (BL.toStrict (messagePut Snapshot {
            dat = fromList (map (uncurry makeDatum) (T.toList db))
          }))) (fromJust storage)
  
  put (req, db, resp, storage)

responder :: HandlerFunc
responder (_, sock, addr) = do
  (req, db, resp, storage) <- get
  case resp of
    Just r -> do
      len <- lift $ sendTo sock (BL.toStrict (messagePut r)) addr
      return ()
    Nothing -> do
      len <- lift $ sendTo sock B.empty addr
      return ()
  put (req, db, resp, storage)

emptyEnv :: Env
emptyEnv = (Nothing, T.empty, Nothing, Nothing)
 
runServer :: String -> [HandlerFunc] -> Env -> String -> IO ()
runServer port handlerfuncs db file =
  void $ withSocketsDo $ runStateT (initServer port handlerfuncs file) db
