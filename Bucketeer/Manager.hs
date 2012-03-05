module Bucketeer.Manager (BucketManager) where

import Bucketeer.Types

import Control.Monad -- (when)
import Control.Concurrent (killThread,
                           ThreadId)
import Data.Maybe (maybe)
import Data.HashMap.Strict (HashMap,
                            insertWith,
                            filterWithKey,
                            delete)

--TODO: just use a flat hash with tuple keys
type BucketDict = HashMap (Consumer, Feature) BucketInterface

data BucketInterface = BucketInterface { bucket :: Bucket,
                                         refillerThread :: Maybe ThreadId}

type BucketManager = BucketDict -- todo

addBucket :: Bucket
             -> BucketManager
             -> BucketManager
addBucket bkt mgr = insertWith replaceBI (cns, feat) newBI mgr
  where feat = feature bkt
        cns  = consumer bkt
        replaceBI _ bi = updateBI bkt bi
        newBI = BucketInterface { bucket = bkt, refillerThread = Nothing }

revokeFeature :: Consumer
                 -> Feature
                 -> BucketManager
                 -> BucketManager
revokeFeature cns feat = delete (cns, feat)

revokeConsumer :: Consumer
                  -> BucketManager
                  -> BucketManager
revokeConsumer cns = filterWithKey keyMatch
  where keyMatch (cns', _) _ = cns' == cns

killBucketInterface :: BucketInterface
                       -> IO ()
killBucketInterface BucketInterface { refillerThread = Just tid} = killThread tid
killBucketInterface _                                            = return ()

---- Helpers
updateBI :: Bucket -> BucketInterface -> BucketInterface
updateBI b bi = bi {bucket = b }
