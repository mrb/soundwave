module Main where

import Soundwave

main :: IO ()
main = do
  putStrLn "[][][] ... [][][]"
  runServer "1514" ["127.0.0.1:1515", "127.0.0.1:1516"] "db.cdb" [parser, router, snapshotter, responder, logger, replicator]
            
