-- From http://book.realworldhaskell.org/read/sockets-and-syslog.html
module Main where

import Data.Bits
import Network.Socket
import Network.BSD
import Data.List

type HandlerFunc = SockAddr -> String -> IO ()

serveLog :: String              -- ^ Port number or name; 514 is default
         -> [HandlerFunc]       -- ^ Function to handle incoming messages
         -> IO ()
serveLog port handlerfuncs = withSocketsDo $
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

main :: IO ()
main = do
  serveLog "1514" [plainHandler, plainHandler']
