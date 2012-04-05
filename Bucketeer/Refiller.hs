module Bucketeer.Refiller (runRefiller) where

import Bucketeer.Types
import Bucketeer.Persistence (restore)

import Control.Concurrent.Thread.Delay (delay)
import Control.Monad (forever)
import Database.Redis (Connection,
                       runRedis)

runRefiller :: Connection
               -> Bucket
               -> IO ()
runRefiller conn Bucket { consumer    = cns,
                          feature     = feat,
                          capacity    = cap,
                          restoreRate = rate} = forever (doRestore >> doDelay)
  where doRestore = runRedis conn $ restore cns feat cap
        doDelay   = delay $ rate * 1000000 -- delay takes microseconds
