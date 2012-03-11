module Bucketeer.Manager (BucketManager(..),
                          buckets,
                          serializeBucketManager,
                          deserializeBucketManager,
                          defaultBucketManager,
                          BucketInterface(..),
                          addBucket,
                          revokeFeature,
                          revokeConsumer) where

import Bucketeer.Util
import Bucketeer.Types

import Control.Applicative ((<$>))
import Control.Concurrent (killThread,
                           ThreadId)

import Data.Aeson.Encode (encode) --TODO: move to Manager
import Data.Aeson.Types (FromJSON,
                         parseJSON,
                         ToJSON,
                         toJSON,
                         object,
                         Value(..),
                         typeMismatch,
                         (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (foldl')
import Data.Maybe (maybe)
import Data.HashMap.Strict (HashMap,
                            empty)

import Data.Text.Encoding (encodeUtf8,
                           decodeUtf8)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H

type BucketManager = BucketDict

buckets :: BucketManager -> [Bucket]
buckets = map bucket . H.elems

serializeBucketManager :: BucketManager
                          -> ByteString
serializeBucketManager = BS.concat . LBS.toChunks . encode . buckets

deserializeBucketManager :: ByteString
                            -> Either String [Bucket]
deserializeBucketManager = undefined

defaultBucketManager :: BucketManager
defaultBucketManager = empty

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

type BucketDict = HashMap (Consumer, Feature) BucketInterface

data BucketInterface = BucketInterface { bucket         :: Bucket,
                                         refillerThread :: ThreadId } deriving (Show, Eq)

