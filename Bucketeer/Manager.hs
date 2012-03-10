module Bucketeer.Manager (BucketManager,
                          BucketInterface(..),
                          addBucket,
                          revokeFeature,
                          revokeConsumer) where

import Bucketeer.Util
import Bucketeer.Types

import Control.Applicative ((<$>))
import Control.Concurrent (killThread,
                           ThreadId)
import Data.List (foldl')
import Data.Maybe (maybe)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H

type BucketDict = HashMap (Consumer, Feature) BucketInterface

data BucketInterface = BucketInterface { bucket         :: Bucket,
                                         refillerThread :: ThreadId } deriving (Show, Eq)

type BucketManager = BucketDict -- todo

addBucket :: Bucket
             -> ThreadId
             -> BucketManager
             -> BucketManager
addBucket bkt tid mgr = H.insertWith replaceBI (cns, feat) newBI mgr
  where feat = feature bkt
        cns  = consumer bkt
        replaceBI _ bi = updateBI bkt bi
        newBI = BucketInterface { bucket = bkt, refillerThread = tid }

revokeFeature :: Consumer
                 -> Feature
                 -> BucketManager
                 -> (BucketManager, Maybe ThreadId)
revokeFeature cns feat bm = (bm', refillerThread <$> maybeBI)
  where (bm', maybeBI) = delete' (cns, feat) bm
        

revokeConsumer :: Consumer
                  -> BucketManager
                  -> (BucketManager, [ThreadId])
revokeConsumer cns bm = foldl' delKey (bm, []) ks
  where ks                   = filter keyMatch $ H.keys bm
        keyMatch             = (==cns) . fst
        delKey (bm', tids) k = case delete' k bm' of
                                (_, Nothing)                                          -> (bm',  tids)
                                (bm'', Just BucketInterface { refillerThread = tid }) -> (bm'', tid:tids)

---- Helpers
updateBI :: Bucket -> BucketInterface -> BucketInterface
updateBI b bi = bi {bucket = b }
