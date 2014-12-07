module Main where

import Control.Concurrent.Async
import Control.Monad.State
import qualified Soundwave.Logger as L
import Soundwave

main :: IO ()
main = do
  putStrLn "[][][] ... [][][]"
  let callbackfuncs = [parser, router, snapshotter, responder, replicator]
  
  l <- liftIO $ L.start
  
  let s1 = runServer "1514" [("127.0.0.1","1515")] "db.cdb" callbackfuncs l
  let s2 = runServer "1515" [("127.0.0.1","1514")] "db15.cdb" callbackfuncs l

  (r1, r2) <- concurrently s1 s2
  return ()
