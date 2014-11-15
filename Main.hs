-- From http://book.realworldhaskell.org/read/sockets-and-syslog.html
--  cabal exec ghci Main.hs 

module Main where
 
import Prelude hiding (getContents)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy
import qualified Network.Socket.ByteString as N
import qualified Data.Map.Strict as M
import Control.Monad.State
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Word
import Data.Int (Int64)
import Data.Sequence (fromList)
import Text.ProtocolBuffers.Header(uFromString)
import Text.ProtocolBuffers.WireMessage (messageGet)
import SoundwaveProtos.Datum
import SoundwaveProtos.Value

type DB = M.Map BL.ByteString [Int]
type HandlerFunc = BL.ByteString -> StateT DB IO DB
 
ins :: BL.ByteString -> [Int] -> DB -> DB
ins = M.insert

d :: Datum
d = Datum {
  name = uFromString "v",
  vector = fromList [
    Value {
      key = 0,
      value = 3
    },
    Value {
      key = 1,
      value = 8
    }
  ]
}

showDatum :: Datum -> String
showDatum d = ""
 
emptyDB :: DB
emptyDB = M.empty
 
serveLog :: String       
         -> [HandlerFunc]
         -> StateT DB IO ()
serveLog port handlerfuncs = do
  do addrinfos <- lift $ getAddrInfo 
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just port)
     let serveraddr = head addrinfos
     sock <- lift $ socket (addrFamily serveraddr) Datagram defaultProtocol
     lift $ bindSocket sock (addrAddress serveraddr)
     processSocket sock handlerfuncs

processSocket :: Socket ->
                 [HandlerFunc] ->
                 StateT DB IO ()
processSocket sock handlerfuncs = do
  msg <- lift $ recv sock 1024
  do
    let (len, datum) = runGet readFramedMessage msg
    if len > 0 then
      do p <- lift $ parseProto datum
         lift $ print $ p
         processSocket sock handlerfuncs
    else 
      processSocket sock handlerfuncs

--where procMessages sock len =
--          do msg <- lift $ recv sock len
--             lift $ print $ msg
--             --mapM_ (\h -> h msg) handlerfuncs
--             processSocket sock handlerfuncs
  

readFramedMessage :: Get (Word32, BL.ByteString)
readFramedMessage = do
  len <- getWord32be
  msg <- getLazyByteString (fromIntegral len)
  return (len, msg)

parseProto :: BL.ByteString -> IO Datum
parseProto s = case messageGet s of
                Right (message, x) | BL.length x == 0 ->
                  return message
                Right (message, x) | BL.length x /= 0 ->
                  error $ "Failed to parse datum"
                Left error_message ->
                  error $ "Failed to parse datum" ++ error_message

dbHandler :: HandlerFunc
dbHandler msg = do
    db <- get
    --put $ ins msg [0] db
    get
 
dbPrinter :: HandlerFunc
dbPrinter msg = do
    db <- get 
    lift $ print msg
    --lift $ print (show db)
    return db
 
runServer :: String -> [HandlerFunc] -> DB -> IO ()
runServer port handlerfuncs db =
  void $ withSocketsDo (runStateT (serveLog port handlerfuncs) db)
 
-- Talk to me with nc -4u localhost 1514 
main :: IO ()
main = do
  putStrLn "[][][] ... [][][]"
  runServer "1514" [dbHandler, dbPrinter] emptyDB

