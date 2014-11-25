--  cabal exec ghci Main.hs 
module Main where
 
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import qualified Data.Map.Strict as M
import Control.Monad.State
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC (pack)
import Data.Binary.Get
import Data.Word
import Data.Foldable (toList)

import Text.Regex.TDFA

import Text.ProtocolBuffers.WireMessage (messageGet)
import Text.ProtocolBuffers.Basic(utf8)
import SoundwaveProtos.Datum
import SoundwaveProtos.Value

type DB = (M.Map BL.ByteString (M.Map Int Int), B.ByteString)
type HandlerFunc = (BL.ByteString, Socket, SockAddr) -> StateT DB IO DB
 
serveLog :: String       
         -> [HandlerFunc]
         -> StateT DB IO ()
serveLog port handlerfuncs = do
     addrinfos <- lift $ getAddrInfo 
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
  (msg, addr) <- lift $ recvFrom sock 1024
  do
    mapM_ (\h -> h ((BL.fromStrict msg), sock, addr)) handlerfuncs
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

messageParser :: HandlerFunc
messageParser (msg, _, _) = do
    (db, resp) <- get
    let (len, datum) = runGet readFramedMessage msg
    p <- lift $ parseProto datum
    let n = utf8 (name p)
    let m = M.fromList (map (\x -> ((fromIntegral (key x)), fromIntegral (value x)))
                            (toList (vector p)))

    if n =~ "%" :: Bool then
      queryDatum n m db resp
    else
      updateDatum n m db resp
    get
    where 
      queryDatum n m db resp = do
        let (b,_,_) = (n =~ "%") :: (BL.ByteString, BL.ByteString, BL.ByteString)
        lift $ putStrLn (show b)
      
      updateDatum n m db resp = if M.member n db then
            do
              let newDb = (M.insert n (M.unionWith max m (db M.! n)) db)
              let newResp = newDb M.! n
              put (newDb, (BC.pack (show newResp)))
          else
            do
              let newDb = (M.insert n m db)
              let newResp = newDb M.! n
              put (newDb, (BC.pack (show newResp)))
 
printer :: HandlerFunc
printer (msg, _, _) = do
    (db, resp) <- get
    lift $ print db
    return (db, resp)

responder :: HandlerFunc
responder (msg, sock, addr) = do
    (db, resp) <- get
    len <- lift $ sendTo sock resp addr
    return (db, resp)
 
runServer :: String -> [HandlerFunc] -> DB -> IO ()
runServer port handlerfuncs db =
  void $ withSocketsDo $ runStateT (serveLog port handlerfuncs) db
 
main :: IO ()
main = do
  putStrLn "[][][] ... [][][]"
  runServer "1514" [messageParser, printer, responder] (M.empty, B.empty)

