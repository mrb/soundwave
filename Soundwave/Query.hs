module Soundwave.Query where

import qualified Soundwave.Logger as L
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Control.Monad.State
import qualified Data.Map.Strict as M
import Data.Int
import Data.Maybe
import Data.Word
import Data.Binary.Put
import qualified Data.Trie as T
import Data.Sequence (fromList)
import Data.Foldable (toList)
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

import Soundwave.Data

queryData :: B.ByteString -> StateT Env IO ()
queryData n = do
  env <- get
  let (b,_,_) = (n =~ "%") :: (B.ByteString, B.ByteString, B.ByteString)
  let matchedDb = T.submap b (db env)
  if T.null matchedDb then
    put env { resp = Nothing }
  else
    do
      let newResp = makeResponse matchedDb
      put env { resp = Just newResp }

aggregateData :: B.ByteString -> StateT Env IO ()
aggregateData n = do
  env <- get
  let (b,_,_) = (n =~ "\\*") :: (B.ByteString, B.ByteString, B.ByteString)
  let matchedDb = T.submap b (db env)
  if T.null matchedDb then
    put env { resp = Nothing }
  else
    do
      let dbi = T.insert n (M.unionsWith max (map snd (T.toList matchedDb))) T.empty
      let newResp = makeResponse dbi
      put env { resp = Just newResp }

updateData :: BL.ByteString -> ValueMap -> StateT Env IO ()
updateData n m  = do
  env <- get
  let ndb = updateDB n m (db env)
  respondAndPut (BL.toStrict n) env { db = ndb }
  where
    respondAndPut key e =
      if T.null (db e) then
        put e
      else
        do
          let r = T.lookup key (db e)
          let respDb = T.insert key (fromJust r) T.empty
          let newResp = makeResponse respDb
          put e { resp = Just newResp }
