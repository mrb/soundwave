-- From http://book.realworldhaskell.org/read/sockets-and-syslog.html
module Main where

import Data.Bits
import Network.Socket
import Network.BSD
import Data.List
import qualified Data.Map.Strict as M
import Control.Monad.State

type DB = M.Map String [Int]
type HandlerFunc = SockAddr -> String -> IO ()

ins :: String -> [Int] -> DB -> DB
ins k v db = M.insert k v db

emptyDB :: DB
emptyDB = M.empty

updateDB :: String -> [Int] -> StateT DB IO DB
updateDB k v = do
  current <- get
  put $ ins k v current
  return current

serveLog :: String              -- ^ Port number or name; 514 is default
         -> [HandlerFunc]       -- ^ Function to handle incoming messages
         -> StateT DB IO ()
serveLog port handlerfuncs = lift $ withSocketsDo $
    do -- Look up the port.  Either raises an exception or returns
       -- a nonempty list.  
       addrinfos <- getAddrInfo 
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just port)
       let serveraddr = head addrinfos

       -- Create a socket
       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

       -- Bind it to the address we're listening to
       bindSocket sock (addrAddress serveraddr)

       -- Loop forever processing incoming data.  Ctrl-C to abort.
       procMessages sock
    where procMessages sock =
              do -- Receive one UDP packet, maximum length 1024 bytes,
                 -- and save its content into msg and its source
                 -- IP and port into addr
                 (msg, _, addr) <- recvFrom sock 1024
                 -- Handle it
                 mapM (\h -> h addr msg) handlerfuncs
                 -- And process more messages
                 procMessages sock

-- A simple handler that prints incoming packets
plainHandler :: HandlerFunc
plainHandler addr msg = 
    putStrLn $ "[A] From " ++ show addr ++ ": " ++ msg

-- Dupe handler to show multiple handlers working with mapM
plainHandler' :: HandlerFunc
plainHandler' addr msg =
    putStrLn $ "[B] From " ++ show addr ++ ": " ++ msg

runServer :: String -> [HandlerFunc] -> DB -> IO ()
runServer port handlerfuncs db = void $ runStateT (serveLog port handlerfuncs) db

main :: IO ()
main = do
  putStrLn "[][][] ... [][][]"
  runServer "1514" [plainHandler, plainHandler'] emptyDB
  --serveLog "1514" [plainHandler, plainHandler']
