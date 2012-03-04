module Bucketeer.Refiller (runRefiller) where

import Bucketeer.Types
import Bucketeer.Persistence (restore)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Thread.Delay (delay)
import Control.Monad (forever)
import Database.Redis (runRedis,
                       connect,
                       ConnectInfo)

runRefiller :: ConnectInfo
               -> Bucket
               -> IO ()
runRefiller ci Bucket { consumer = cns,
                        feature = feat,
                        capacity = cap,
                        restoreRate = rate} = loop =<< connect ci
  where loop conn      = forever $ (doRestore conn >> doDelay)
        doRestore conn = runRedis conn $ restore cns feat cap
        doDelay        = delay $ rate * 1000000 -- delay takes microseconds
