-- From http://book.realworldhaskell.org/read/sockets-and-syslog.html
module Main where
 
import Network.Socket
import qualified Data.Map.Strict as M
import Control.Monad.State
 
type DB = M.Map String [Int]
type HandlerFunc = SockAddr -> String -> StateT DB IO DB
 
ins :: String -> [Int] -> DB -> DB
ins = M.insert
 
emptyDB :: DB
emptyDB = M.empty
 
serveLog :: String       
         -> [HandlerFunc]
         -> StateT DB IO ()
serveLog port handlerfuncs = do
  db <- get
  do addrinfos <- lift $ getAddrInfo 
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just port)
     let serveraddr = head addrinfos
     sock <- lift $ socket (addrFamily serveraddr) Datagram defaultProtocol
     lift $ bindSocket sock (addrAddress serveraddr)
     procMessages sock
  where procMessages sock =
            do (msg, _, addr) <- lift $ recvFrom sock 1024
               mapM_ (\h -> h addr msg ) handlerfuncs
               procMessages sock
 
dbHandler :: HandlerFunc
dbHandler addr msg = do
    db <- get
    put $ ins msg [0] db
    get
 
dbPrinter :: HandlerFunc
dbPrinter addr msg = do
    db <- get 
    lift $ print (show db)
    return db
 
runServer :: String -> [HandlerFunc] -> DB -> IO ()
runServer port handlerfuncs db =
  void $ withSocketsDo (runStateT (serveLog port handlerfuncs) db)
 
main :: IO ()
main = do
  putStrLn "[][][] ... [][][]"
  runServer "1514" [dbHandler, dbPrinter] emptyDB
