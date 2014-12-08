module Soundwave.Persistence where

import Soundwave.Data
import qualified Soundwave.Logger as L
import SoundwaveProtos.Datum
import SoundwaveProtos.Value
import SoundwaveProtos.Request
import SoundwaveProtos.Response
import SoundwaveProtos.Snapshot
import Text.ProtocolBuffers.WireMessage (messageGet, messagePut)
import Text.ProtocolBuffers.Basic(utf8, uFromString, toUtf8)
import Control.Monad.State
import Data.Sequence (fromList)
import Data.Maybe
import qualified Data.Trie as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC (pack, unpack)
import qualified Data.Map.Strict as M
import Data.Foldable (toList)
import Control.Arrow ((&&&)) -- thanks, hlint!
import System.Directory (doesFileExist)
import Database.PureCDB

initPersistence :: FilePath -> StateT Env IO ()
initPersistence file = do
   env <- get
   fileExists <- lift $ doesFileExist file
   if fileExists then 
     do
       lift $ L.info (logger env) ("Loading existing db: " ++ file)
       r <- liftIO $ openCDB file
       bs <- liftIO $ getBS r (BC.pack "dbstate")
       snapshot <- liftIO $ parseSnapshotBytes (B.concat bs)
       let newDb = snapshotToDB snapshot
       lift $ L.info (logger env) ("Loaded " ++ show (length (toList (dat snapshot))) ++ " keys.")
       put env { storage = Just file, db = newDb }
    else
      do
        makeCDB (addBS (BC.pack "dbstate") (BL.toStrict (messagePut Snapshot {
            dat = fromList (map (uncurry makeDatum) (T.toList (db env)))
          }))) file
        put env { storage = Just file }
   return ()

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

saveSnapshot :: DB -> FilePath -> IO ()
saveSnapshot db storage = makeCDB (addBS (BC.pack "dbstate") (BL.toStrict (messagePut Snapshot {
            dat = fromList (map (uncurry makeDatum) (T.toList db))
          }))) storage
