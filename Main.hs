module Main where

import Control.Concurrent.Async
import Soundwave

main :: IO ()
main = do
  putStrLn "[][][] ... [][][]"
  let callbackfuncs = [parser, router, snapshotter, responder, replicator]
  
  let s1 = runServer "1514" [("mrb.local","1515")] "db.cdb" callbackfuncs
  let s2 = runServer "1515" [("mrb.local","1514")] "db15.cdb" callbackfuncs

  (r1, r2) <- concurrently s1 s2
  return ()
