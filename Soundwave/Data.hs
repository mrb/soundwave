module Soundwave.Data where

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

type ValueMap = (M.Map Int32 Int32)
type DB = T.Trie ValueMap
type Cluster = [AddrInfo]

data Env = Env { 
  db      :: DB
, req     :: Maybe Request
, resp    :: Maybe Response
, storage :: Maybe Storage
, cluster :: Maybe Cluster
, logger  :: L.Log
}

type HandlerFunc = (BL.ByteString, Socket, SockAddr) -> StateT Env IO ()
type Storage = String

data RequestType = Update | Query | Aggregate | PeerContact deriving (Show)
data Node = Node SockAddr Socket

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

makeDatum :: B.ByteString -> M.Map Int32 Int32 -> Datum
makeDatum k v = Datum { 
  name = uFromString (BC.unpack k),
  vector = fromList (map (uncurry Value) (M.toList v))
}

makeResponse :: DB -> Response
makeResponse db =  Response { 
  response = fromList (map (uncurry makeDatum) (T.toList db))
}

makeRequest :: DB -> Request
makeRequest db = Request {
  request = head $ (map (uncurry makeDatum) (T.toList db))
}

updateDB :: BL.ByteString -> ValueMap -> DB -> DB
updateDB k v db = if T.member (BL.toStrict k) db then
    T.insert (BL.toStrict k) (M.unionWith max v (fromJust $ T.lookup (BL.toStrict k) db)) db
  else
    T.insert (BL.toStrict k) v db

frameMessage :: Word32 -> B.ByteString -> PutM ()
frameMessage l m = do putWord32be l
                      putByteString m

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
