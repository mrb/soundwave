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

import Text.Regex.TDFA

import Text.ProtocolBuffers.WireMessage (messageGet, messagePut)
import Text.ProtocolBuffers.Basic(utf8, uFromString)
import SoundwaveProtos.Datum
import SoundwaveProtos.Value
import SoundwaveProtos.Request
import SoundwaveProtos.Response

type ValueMap = (M.Map Int32 Int32)
type DB = M.Map BL.ByteString ValueMap
type Env = (DB, Maybe Response)
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

parseProto :: B.ByteString -> IO Datum
parseProto s = case messageGet (BL.fromStrict s) of
                Right (message, x) | BL.length x == 0 ->
                  return message
                Right (message, x) | BL.length x /= 0 ->
                  error "Failed to parse datum"
                Left error_message ->
                  error $ "Failed to parse datum" ++ error_message

makeDatum :: BL.ByteString -> M.Map Int32 Int32 -> Datum
makeDatum k v = Datum { 
  name = uFromString (BC.unpack (BL.toStrict k)),
  vector = fromList (map (uncurry Value) (M.toList v))
}

dbToData :: DB -> [Datum]
dbToData db = map (uncurry makeDatum) (M.toList db)

dbToByteString :: DB -> B.ByteString
dbToByteString db = B.concat $ map (BL.toStrict . messagePut) (dbToData db)

makeResponse :: DB -> Response
makeResponse db =  Response { 
  response = fromList (dbToData db)
}

messageParser :: HandlerFunc
messageParser (msg, _, _) = do
  (db, resp) <- get
  let (len, datum) = runGet readFramedMessage msg
  p <- lift $ parseProto datum
  let n = utf8 (name p)
  let m = M.fromList (map (\x -> (fromIntegral (key x), fromIntegral (value x)))
                          (toList (vector p)))

  if n =~ "%" :: Bool then
    queryDatum n m db resp
  else if n =~ "\\*" :: Bool then
    queryDatum n m db resp
  else
    updateDatum n m db resp
  get

queryDatum :: BL.ByteString -> ValueMap -> DB -> Maybe Response -> StateT Env IO ()
queryDatum n m db resp = do
  let (b,_,_) = (n =~ "%") :: (BL.ByteString, BL.ByteString, BL.ByteString)
  if M.member b db then
      respondAndPut b db resp
    else
      respondAndPut n M.empty resp

updateDatum :: BL.ByteString -> ValueMap -> DB -> Maybe Response -> StateT Env IO ()
updateDatum n m db resp = if M.member n db then
      respondAndPut n (M.insert n (M.unionWith max m (db M.! n)) db) resp
    else
      respondAndPut n (M.insert n m db) resp

respondAndPut :: BL.ByteString -> DB -> Maybe Response -> StateT Env IO ()
respondAndPut key newDb resp =
  if M.null newDb then
    put (newDb, resp)
  else
    do
      let respDb = M.insert key (newDb M.! key) M.empty
      let newResp = makeResponse respDb
      put (newDb, Just newResp)
 
printer :: HandlerFunc
printer (msg, _, _) = do
    (db, resp) <- get
    lift $ print ("[DB STATE] " ++ show db)
    return (db, resp)

responder :: HandlerFunc
responder (msg, sock, addr) = do
  (db, resp) <- get
  case resp of
    Just (Response r) -> do
      len <- lift $ sendTo sock (dbToByteString db) addr
      return (db, resp)
    Nothing -> do
      len <- lift $ sendTo sock B.empty addr
      return (db, resp)
 
runServer :: String -> [HandlerFunc] -> Env -> IO ()
runServer port handlerfuncs db =
  void $ withSocketsDo $ runStateT (initServer port handlerfuncs) db
 
main :: IO ()
main = do
  putStrLn "[][][] ... [][][]"
  runServer "1514" [messageParser, printer, responder] (M.empty, Nothing)
