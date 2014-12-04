module Main where

import Soundwave

main :: IO ()
main = do
  putStrLn "[][][] ... [][][]"
  runServer "1514" [requestParser, requestRouter, logger, snapshotter, responder] emptyEnv "db.cdb"
