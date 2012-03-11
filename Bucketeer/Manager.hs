module Bucketeer.Manager (BucketManager(..),
                          bmToSBM,
                          defaultBucketManager,
                          SerializedBucketManager(..),
                          BucketInterface(..),
                          addBucket,
                          revokeFeature,
                          revokeConsumer) where

import Bucketeer.Util
import Bucketeer.Types

import Control.Applicative ((<$>))
import Control.Concurrent (killThread,
                           ThreadId)
import Data.Aeson.Types (FromJSON,
                         parseJSON,
                         ToJSON,
                         toJSON,
                         object,
                         Value(..),
                         typeMismatch,
                         (.=))
import Data.List (foldl')
import Data.Maybe (maybe)
import Data.HashMap.Strict (HashMap,
                            empty)

import Data.Text.Encoding (encodeUtf8,
                           decodeUtf8)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H

type BucketManager = BucketDict

bmToSBM :: BucketManager -> SerializedBucketManager
bmToSBM bm = SerializedBucketManager $ H.toList byCns
  where bmKeys = H.keys bm
        byCns  = foldl' insertPair H.empty bmKeys
        insertPair h (cns, feat) = H.insertWith (++) cns [feat] h

newtype SerializedBucketManager = SerializedBucketManager [(Consumer, [Feature])] deriving (Eq, Show)

instance ToJSON SerializedBucketManager where
  toJSON (SerializedBucketManager bs) = object $ foldl' prependBucket [] bs
    where prependBucket pairs (Consumer cns, feats) = (decodeUtf8 cns .=  map convertFeat feats):pairs
          convertFeat (Feature feat)                = decodeUtf8 feat

instance FromJSON SerializedBucketManager where
  parseJSON (Object obj) = return $ H.foldlWithKey' tuple (SerializedBucketManager []) obj
    where tuple (SerializedBucketManager bs) txtCns (Array featVals) = SerializedBucketManager $ (convertCns txtCns, extractFeatures featVals):bs
          tuple (SerializedBucketManager bs) txtCns _                = SerializedBucketManager bs
          extractFeatures = V.foldl' collectIfText []
          collectIfText feats (String feat) = (Feature . encodeUtf8 $ feat):feats
          collectIfText feats _             = feats
          convertCns = Consumer . encodeUtf8
  parseJSON v            = typeMismatch "Object" v

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

