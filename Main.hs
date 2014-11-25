--  cabal exec ghci Main.hs 
module Main where
 
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import qualified Data.Map.Strict as M
import Control.Monad.State
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Word
import Data.Foldable (toList)

import Text.ProtocolBuffers.WireMessage (messageGet)
import Text.ProtocolBuffers.Basic(utf8)
import SoundwaveProtos.Datum
import SoundwaveProtos.Value

type DB = M.Map BL.ByteString (M.Map Int Int)
type HandlerFunc = BL.ByteString -> StateT DB IO DB
 
ins :: BL.ByteString -> M.Map Int Int -> DB -> DB
ins = M.insert
 
emptyDB :: DB
emptyDB = M.empty
 
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
    mapM_ (\h -> h (BL.fromStrict msg)) handlerfuncs
    processSocket sock handlerfuncs

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

protoParser :: HandlerFunc
protoParser msg = do
    db <- get
    let (len, datum) = runGet readFramedMessage msg
    p <- lift $ parseProto datum
    lift $ print p
    let n = utf8 (name p)
   
    let m = M.fromList (map (\x -> (fromIntegral (key x), fromIntegral (value x)))
                            (toList (vector p)))

    if M.member n db then
      put $ ins n (M.unionWith max m (db M.! n)) db
    else
      put $ ins n m db

    get
 
printer :: HandlerFunc
printer msg = do
    db <- get 
    lift $ print db
    return db
 
runServer :: String -> [HandlerFunc] -> DB -> IO ()
runServer port handlerfuncs db =
  void $ withSocketsDo (runStateT (serveLog port handlerfuncs) db)
 
main :: IO ()
main = do
  putStrLn "[][][] ... [][][]"
  runServer "1514" [protoParser, printer] emptyDB

