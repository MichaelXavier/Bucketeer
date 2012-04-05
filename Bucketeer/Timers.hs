module Bucketeer.Timers (startBucketManager) where

import Bucketeer.Manager
import Bucketeer.Refiller (runRefiller)
import Bucketeer.Types

import Control.Concurrent (forkIO)
import Data.List(foldl')
import Database.Redis (Connection)

startBucketManager :: [Bucket]
                      -> Connection
                      -> IO BucketManager
startBucketManager bkts conn = do threads <- mapM startRefiller bkts
                                  return . setupBM $ zip bkts threads
  where startRefiller = forkIO . runRefiller conn
        setupBM       = foldl' (uncurry . bucketAdd) newBM
        newBM         = defaultBucketManager
        bucketAdd mgr bkt tid = addBucket bkt tid mgr


