module Main where

import Control.Concurrent.Async
import Control.Monad.State
import Soundwave.Server
import qualified Soundwave.Logger as L

main :: IO ()
main = do
  putStrLn "[][][] ... [][][]"
  let callbackfuncs = [parser, router, snapshotter, responder, replicator]
  
  l <- liftIO $ L.start
  
  let s1 = runServer "1514" [("127.0.0.1","1514"), ("127.0.0.1","1515")] "db14.cdb" callbackfuncs l
  let s2 = runServer "1515" [("127.0.0.1","1515"), ("127.0.0.1","1516")] "db15.cdb" callbackfuncs l
  let s3 = runServer "1516" [("127.0.0.1","1514"), ("127.0.0.1","1515")] "db16.cdb" callbackfuncs l

  (r1, r2) <- concurrently s1 $ (concurrently s2 s3)
  return ()
