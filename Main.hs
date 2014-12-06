module Main where

import Control.Concurrent.Async
import Soundwave

main :: IO ()
main = do
  putStrLn "[][][] ... [][][]"
  let callbackfuncs = [parser, router, snapshotter, responder, logger, replicator]
  
  let s1 = runServer "1514" [("127.0.0.1","1515")] "db.cdb" callbackfuncs
  let s2 = runServer "1515" [("127.0.0.1","1514")] "db15.cdb" callbackfuncs

  (r1, r2) <- concurrently s1 s2
  return ()
