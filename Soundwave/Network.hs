module Soundwave.Network where

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

makeAddr :: String -> String -> StateT Env IO AddrInfo
makeAddr host port = do
  let config = (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
  addrinfos <- lift $ getAddrInfo config (Just host) (Just port)
  let serveraddr = head addrinfos
  return serveraddr

connectSocket :: AddrInfo -> StateT Env IO Socket
connectSocket addr = do
  sock <- lift $ socket (addrFamily addr) Datagram defaultProtocol
  lift $ bindSocket sock (addrAddress addr)
  return sock


processSocket :: Socket ->
                 [HandlerFunc] ->
                 StateT Env IO ()
processSocket sock handlerfuncs = do
  (msg, addr) <- lift $ recvFrom sock 4096
  do
    mapM_ (\h -> h (BL.fromStrict msg, sock, addr)) handlerfuncs
    processSocket sock handlerfuncs
  return ()
