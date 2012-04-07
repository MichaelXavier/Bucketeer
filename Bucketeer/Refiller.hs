module Bucketeer.Refiller (runRefiller) where

import Bucketeer.Types
import Bucketeer.Persistence (restore)

import Control.Concurrent.Thread.Delay (delay)
import Control.Monad (forever)
import Database.Redis (Connection,
                       runRedis)

runRefiller :: BucketeerNamespace
               -> Connection
               -> Bucket
               -> IO ()
runRefiller ns conn Bucket { consumer    = cns,
                             feature     = feat,
                             capacity    = cap,
                             restoreRate = rate} = forever (doRestore >> doDelay)
  where doRestore = runRedis conn $ restore ns cns feat cap
        doDelay   = delay $ rate * 1000 -- delay takes microseconds
