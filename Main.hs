--  cabal exec ghci Main.hs 
module Main where
 
import Prelude hiding (getContents)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy

import qualified Data.Map.Strict as M
import Control.Monad.State
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Word
import Data.Foldable (toList)

import Text.ProtocolBuffers.WireMessage (messageGet)
import Text.ProtocolBuffers.Basic(utf8)
import SoundwaveProtos.Datum
import SoundwaveProtos.Value

import Text.Regex.TDFA

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
  msg <- lift $ recv sock 1024
  do
    mapM_ (\h -> h msg) handlerfuncs
    processSocket sock handlerfuncs

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
                  error "Failed to parse datum"
                Left error_message ->
                  error $ "Failed to parse datum" ++ error_message

messageParser :: HandlerFunc
messageParser msg = do
    db <- get
    let (len, datum) = runGet readFramedMessage msg
    p <- lift $ parseProto datum

    let n = utf8 (name p)
    let m = M.fromList (map (\x -> ((fromIntegral (key x)), fromIntegral (value x)))
                            (toList (vector p)))

    if n =~ "%" :: Bool then
      queryDatum n m db
    else
      updateDatum n m db 
    get

queryDatum n m db = do
  let (b,_,_) = ((n =~ "%") :: (BL.ByteString, BL.ByteString, BL.ByteString))
  lift $ putStrLn (show b)

updateDatum n m db = if M.member n db then
      put $ ins n (M.unionWith max m (db M.! n)) db
    else
      put $ ins n m db

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
  runServer "1514" [messageParser, printer] emptyDB

