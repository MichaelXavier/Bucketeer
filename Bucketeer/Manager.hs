{-# LANGUAGE OverloadedStrings #-}
module Bucketeer.Manager (BucketManager(..),
                          buckets,
                          startBucketManager,
                          serializeBucketManager,
                          deserializeBucketManager,
                          storeBucketManager,
                          restoreBuckets,
                          defaultBucketManager,
                          BucketInterface(..),
                          addBucket,
                          revokeFeature,
                          revokeConsumer) where

import Bucketeer.Persistence (restore)
import Bucketeer.Types
import Bucketeer.Util

import Control.Applicative ((<$>))
import Control.Concurrent (killThread,
                           forkIO,
                           ThreadId)
import Control.Concurrent.Thread.Delay (delay)
import Control.Monad (forever)
import Data.Aeson.Encode (encode)
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
import Database.Redis (Connection,
                       Redis,
                       set,
                       get,
                       runRedis)

type BucketManager = BucketDict

buckets :: BucketManager -> [Bucket]
buckets = map bucket . H.elems

serializeBucketManager :: BucketManager
                          -> ByteString
serializeBucketManager = BS.concat . LBS.toChunks . encode . buckets

deserializeBucketManager :: ByteString
                            -> Either String [Bucket]
deserializeBucketManager = decodeJSON

startBucketManager :: [Bucket]
                      -> Connection
                      -> IO BucketManager
startBucketManager buckets conn = do threads <- mapM startRefiller buckets
                                     return . setupBM $ zip buckets threads
  where startRefiller = forkIO . runRefiller conn
        setupBM       = foldl' (uncurry . bucketAdd) newBM
        newBM         = defaultBucketManager
        bucketAdd mgr bkt tid = addBucket bkt tid mgr



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

storeBucketManager :: BucketManager
                      -> Redis ()
storeBucketManager bm = set managerKey serialized  >> return ()
  where serialized = serializeBucketManager bm

restoreBuckets :: Redis (Either String [Bucket])
restoreBuckets = return . loadBM =<< get managerKey
  where loadBM (Left _)        = Left "Redis returned unexpected response"
        loadBM (Right Nothing)   = Left "Could not find manager at bucketeer:manager"
        loadBM (Right (Just bs)) = deserializeBucketManager bs


---- Helpers
updateBI :: Bucket -> BucketInterface -> BucketInterface
updateBI b bi = bi {bucket = b }

type BucketDict = HashMap (Consumer, Feature) BucketInterface

data BucketInterface = BucketInterface { bucket         :: Bucket,
                                         refillerThread :: ThreadId } deriving (Show, Eq)

runRefiller :: Connection
               -> Bucket
               -> IO ()
runRefiller conn Bucket { consumer    = cns,
                          feature     = feat,
                          capacity    = cap,
                          restoreRate = rate} = loop conn
  where loop conn      = forever $ (doRestore conn >> doDelay)
        doRestore conn = runRedis conn $ restore cns feat cap
        doDelay        = delay $ rate * 1000000 -- delay takes microseconds

managerKey :: ByteString
managerKey = "bucketeer:manager"
