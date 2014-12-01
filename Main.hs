module Main where
 
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

import qualified Data.Trie as T

type ValueMap = (M.Map Int32 Int32)
type DB =  T.Trie ValueMap
type Env = (Maybe Request, DB, Maybe Response)
type HandlerFunc = (BL.ByteString, Socket, SockAddr) -> StateT Env IO Env

initServer :: String
         -> [HandlerFunc]
         -> StateT Env IO ()
initServer port handlerfuncs = do
 addrinfos <- lift $ getAddrInfo
                (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                Nothing (Just port)
 let serveraddr = head addrinfos
 sock <- lift $ socket (addrFamily serveraddr) Datagram defaultProtocol
 lift $ bindSocket sock (addrAddress serveraddr)
 processSocket sock handlerfuncs

processSocket :: Socket ->
                 [HandlerFunc] ->
                 StateT Env IO ()
processSocket sock handlerfuncs = do
  (msg, addr) <- lift $ recvFrom sock 1024
  do
    mapM_ (\h -> h (BL.fromStrict msg, sock, addr)) handlerfuncs
    processSocket sock handlerfuncs
  return ()

readFramedMessage :: Get (Word32, B.ByteString)
readFramedMessage = do
  len <- getWord32be
  msg <- getByteString (fromIntegral len)
  return (len, msg)

parseRequestBytes :: B.ByteString -> IO Datum
parseRequestBytes s = case messageGet (BL.fromStrict s) of
                Right (message, x) | BL.length x == 0 ->
                  return (request message)
                Right (message, x) | BL.length x /= 0 ->
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

messageParser :: HandlerFunc
messageParser (msg, _, _) = do
  (req, db, resp) <- get
  let (len, request) = runGet readFramedMessage msg
  p <- lift $ parseRequestBytes request
  let n = utf8 (name p)
  let m = M.fromList (map (\x -> (fromIntegral (key x), fromIntegral (value x)))
                          (toList (vector p)))

  if n =~ "%" :: Bool then
    queryData (BL.toStrict n) m (req, db, resp)
  else if n =~ "\\*" :: Bool then
    queryData (BL.toStrict n) m (req, db, resp)
  else
    updateData n m (req, db, resp)
  get

queryData :: B.ByteString -> ValueMap -> Env -> StateT Env IO ()
queryData n m (req, db, resp) = do
  let (b,_,_) = (n =~ "%") :: (B.ByteString, B.ByteString, B.ByteString)
  let matchedDb = (T.submap b db)
  if T.null matchedDb then
    put (req, db, Nothing)
  else
    do
      let newResp = makeResponse matchedDb
      put (req, db, (Just newResp))

updateData :: BL.ByteString -> ValueMap -> Env -> StateT Env IO ()
updateData n m (req, db, resp) = if T.member (BL.toStrict n) db then
      do
        let newDb = (T.insert (BL.toStrict n) (M.unionWith max m (fromJust (T.lookup (BL.toStrict n) db))) db)
        respondAndPut (BL.toStrict n) (req, newDb, resp)
    else
      do
        let newDb = T.insert (BL.toStrict n) m db
        let respDb = T.insert (BL.toStrict n) m T.empty
        let newResp = makeResponse respDb
        respondAndPut (BL.toStrict n) (req, newDb, (Just newResp))

respondAndPut :: B.ByteString -> Env -> StateT Env IO ()
respondAndPut key (req, newDb, resp) =
  if T.null newDb then
    put (req, newDb, resp)
  else
    do
      let r = T.lookup key newDb
      let respDb = T.insert key (fromJust r) T.empty
      let newResp = makeResponse respDb
      put (req, newDb, (Just newResp))
 
printer :: HandlerFunc
printer (msg, _, _) = do
    (req, db, resp) <- get
    lift $ print ("[REQ] " ++ show req ++ " [DB STATE] " ++ show db ++ " [RESP] " ++ show resp)
    return (req, db, resp)

responder :: HandlerFunc
responder (msg, sock, addr) = do
  (req, db, resp) <- get
  case resp of
    Just r -> do
      len <- lift $ sendTo sock (BL.toStrict (messagePut r)) addr
      return (req, db, resp)
    Nothing -> do
      len <- lift $ sendTo sock B.empty addr
      return (req, db, resp)
 
runServer :: String -> [HandlerFunc] -> Env -> IO ()
runServer port handlerfuncs db =
  void $ withSocketsDo $ runStateT (initServer port handlerfuncs) db
 
main :: IO ()
main = do
  putStrLn "[][][] ... [][][]"
  runServer "1514" [messageParser, printer, responder] (Nothing, T.empty, Nothing)
