{-# LANGUAGE OverloadedStrings #-}
module Bucketeer.Manager (BucketManager,
                          buckets,
                          serializeBucketManager,
                          deserializeBucketManager,
                          defaultBucketManager,
                          BucketInterface(..),
                          addBucket,
                          replaceBucket,
                          revokeFeature,
                          featureExists,
                          consumerExists,
                          revokeConsumer) where

import Bucketeer.Types
import Bucketeer.Util

import Control.Applicative ((<$>))
import Control.Concurrent (ThreadId)
import Data.Aeson.Encode (encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (foldl')
import Data.HashMap.Strict (HashMap,
                            empty)
import qualified Data.HashMap.Strict as H

type BucketManager = BucketDict

buckets :: BucketManager -> [Bucket]
buckets = map bucket . H.elems

serializeBucketManager :: BucketManager
                          -> ByteString
serializeBucketManager = BS.concat . LBS.toChunks . encode . buckets

deserializeBucketManager :: ByteString
                            -> Either String [Bucket]
deserializeBucketManager = decodeJSON


defaultBucketManager :: BucketManager
defaultBucketManager = empty

addBucket :: Bucket
             -> ThreadId
             -> BucketManager
             -> BucketManager
addBucket bkt tid = H.insertWith replaceBI (cns, feat) newBI
  where feat        = feature bkt
        cns         = consumer bkt
        replaceBI _ = updateBI bkt
        newBI       = BucketInterface { bucket = bkt, refillerThread = tid }

replaceBucket :: Bucket
                 -> ThreadId
                 -> BucketManager
                 -> (BucketManager, Maybe ThreadId)
replaceBucket bkt@Bucket { consumer = cns,
                           feature  = feat} tid bm = (bmFinal, maybeTid)
  where (bm', maybeTid) = revokeFeature cns feat bm
        bmFinal         = addBucket bkt tid bm'
        

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

featureExists :: Consumer
                 -> Feature
                 -> BucketManager
                 -> Bool
featureExists = curry H.member

consumerExists :: Consumer
                  -> BucketManager
                  -> Bool
consumerExists cns = any cnsMatch . H.keys
  where cnsMatch = (==cns) . fst

---- Helpers
updateBI :: Bucket -> BucketInterface -> BucketInterface
updateBI b bi = bi {bucket = b }

type BucketDict = HashMap (Consumer, Feature) BucketInterface

data BucketInterface = BucketInterface { bucket         :: Bucket,
                                         refillerThread :: ThreadId } deriving (Show, Eq)
