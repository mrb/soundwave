-- cribbed from https://github.com/pyr/apotiki/blob/master/System/Apotiki/Logger.hs
module Soundwave.Logger (start, info, Log) where

import System.IO (openFile, hPutStrLn, hFlush, hClose,
                  IOMode (AppendMode), Handle)
import Control.Concurrent (forkIO)
import Control.Monad (void, forever)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan (writeTChan, readTChan, newTChanIO, TChan)

type Log = TChan String

logstdout :: TChan String -> IO ()
logstdout chan = do
  line <- atomically $ readTChan chan
  putStrLn line

start :: IO (TChan String)
start = do
  chan <- newTChanIO
  void $ forkIO $ forever $ logstdout chan
  return chan

info :: TChan String -> String -> IO ()
info chan msg = do
  let info_msg = "[info] " ++ msg
  atomically $ writeTChan chan info_msg
