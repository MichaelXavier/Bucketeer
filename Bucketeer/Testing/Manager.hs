{-# LANGUAGE OverloadedStrings #-}
module Bucketeer.Testing.Manager (specs) where

import Bucketeer.Manager
import Bucketeer.Testing.TestHelpers
import Bucketeer.Types
import Bucketeer.Util

import Control.Concurrent (myThreadId,
                           ThreadId)
import Data.HashMap.Strict (empty,
                            insert,
                            singleton)
import Data.List (foldl')
import Test.Hspec (Specs,
                   describe,
                   descriptions,
                   pending,
                   it)
import Test.Hspec.HUnit
import Test.HUnit.Base ((~?=))

specs :: IO Specs
specs = do tid <- myThreadId
           return $ descriptions [describe_addBucket tid,
                                  describe_revokeFeature tid,
                                  describe_revokeConsumer tid,
                                  describe_SerializedBucketManager_fromJSON,
                                  describe_SerializedBucketManager_toJSON]

describe_addBucket :: ThreadId
                      -> Specs
describe_addBucket tid = describe "Bucketeer.Manager.addBucket" [
    it "key doesn't exist: adds the key"         $
      addBucket bkt tid bm ~?= bm' tid,
    it "key already exists leaves the key alone" $
      addBucket bkt tid (bm' tid) ~?= bm' tid
  ]

describe_revokeFeature :: ThreadId
                          -> Specs
describe_revokeFeature tid = describe "Bucketeer.Manager.revokeFeature" [
    it "key doesn't exist: return unaltered BM and Nothing" $
      revokeFeature cns feat bm ~?= (bm, Nothing),
    it "key exists: return altered BM and the thread id" $
      revokeFeature cns feat (bm' tid) ~?= (bm, Just tid)
  ]

describe_revokeConsumer :: ThreadId
                           -> Specs
describe_revokeConsumer tid = describe "Bucketeer.Manager.revokeConsumer" [
    it "consumer does not exist: returns unaltered BM and empty list" $
      revokeConsumer (Consumer "bill") bm'' ~?= (bm'', []),
    it "consumer exists: clears consumer's features, returns the thread ids" $
      revokeConsumer cns bm'' ~?= (onlyJoe, [tid, tid])
  ]
  where bm'' :: BucketManager
        bm''    = foldl' (\h k -> insert k (bi tid) h) bm [(cns, feat),
                                                           (cns, Feature "kill_me"),
                                                           (Consumer "joe", Feature "spare_me")]
        onlyJoe = singleton (Consumer "joe", Feature "spare_me") $ bi tid

describe_SerializedBucketManager_fromJSON :: Specs
describe_SerializedBucketManager_fromJSON = describe "Bucketeer.Manager.SerializedBucketManager fromJSON" [
    it "parses an empty object"           $
      parseJSON "{}" ~?= Right emptySBM,
    it "parses a valid, non-empty object" $
      parseJSON "{\"summer\":[\"barrel_roll\",\"wat\"]}" ~?= Right (fullSBM),
    it "parses the wrong type"            $
      (parseJSON "[]" :: Either String SerializedBucketManager) ~?= Left "when expecting a Object, encountered Array instead"
  ]

describe_SerializedBucketManager_toJSON :: Specs
describe_SerializedBucketManager_toJSON = describe "Bucketeer.Manager.SerializedBucketManager toJSON" [
    it "serializes an empty SerializedBucketManager"                          $
      toJSONText emptySBM ~?= "{}",
    it "serializes a non-empty SerializedBucketManager, not preserving order" $
      toJSONText fullSBM ~?= "{\"summer\":[\"wat\",\"barrel_roll\"]}"
  ]

---- Helpers
bm :: BucketManager
bm = empty

bm' :: ThreadId -> BucketManager
bm' tid = singleton (cns, feat) $ bi tid

bi :: ThreadId -> BucketInterface
bi tid = BucketInterface { bucket         = bkt,
                           refillerThread = tid }


bkt :: Bucket
bkt = Bucket { consumer    = cns,
               feature     = feat,
               capacity    = 2,
               restoreRate = 1 }

cns :: Consumer
cns = Consumer "summer"

feat :: Feature
feat = Feature "barrel_roll"

emptySBM :: SerializedBucketManager
emptySBM = SerializedBucketManager []

fullSBM :: SerializedBucketManager
fullSBM = SerializedBucketManager [(cns, [Feature "wat", feat])]
