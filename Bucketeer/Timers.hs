module Bucketeer.Timers (startBucketManager) where

import Bucketeer.Manager
import Bucketeer.Refiller (runRefiller)
import Bucketeer.Types

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Data.List(foldl')
import Database.Redis (Connection)

startBucketManager :: [Bucket]
                      -> BucketeerNamespace
                      -> Connection
                      -> IO BucketManager
startBucketManager bkts ns conn = do threads <- mapM startRefiller bkts
                                     return . setupBM $ zip bkts threads
  where startRefiller = forkIO . runRefiller ns conn
        setupBM       = foldl' (uncurry . bucketAdd) newBM
        newBM         = defaultBucketManager
        bucketAdd mgr bkt tid = addBucket bkt tid mgr


