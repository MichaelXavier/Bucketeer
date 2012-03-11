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
                                  describe_SerializedBucketManager_toJSON,
                                  describe_bmToSBM tid]

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
      parseJSON "{\"summer\":[\"wat\",\"barrel_roll\"]}" ~?= Right (fullSBM),
    it "parses the wrong type"            $
      (parseJSON "[]" :: Either String SerializedBucketManager) ~?= Left "when expecting a Object, encountered Array instead"
  ]

describe_SerializedBucketManager_toJSON :: Specs
describe_SerializedBucketManager_toJSON = describe "Bucketeer.Manager.SerializedBucketManager toJSON" [
    it "serializes an empty SerializedBucketManager"                          $
      toJSONText emptySBM ~?= "{}",
    it "serializes a non-empty SerializedBucketManager, not preserving order" $
      toJSONText fullSBM ~?= "{\"summer\":[\"barrel_roll\",\"wat\"]}"
  ]

describe_bmToSBM :: ThreadId
                    -> Specs
describe_bmToSBM tid = describe "Bucketeer.Manager.bmToSBM" [
    it "generates an empty SBM from an empty BM" $
      bmToSBM bm ~?= emptySBM,
    it "generates a populated SBM from a populated BM" $
      bmToSBM (fullBM tid) ~?= fullSBM,
    it "handles a multi-consumer BM" $
      bmToSBM (multiBM tid) ~?= multiSBM
  ]
  where fullBM tid = foldl' (\h feat -> insert (cns, feat) (bi tid) h) bm [wat, feat]
        multiBM tid = foldl' (\h k -> insert k (bi tid) h) bm [(cns, feat),
                                                               (bob, feat),
                                                               (bob, wat)]

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

bob :: Consumer
bob = Consumer "bob"

feat :: Feature
feat = Feature "barrel_roll"

wat :: Feature
wat = Feature "wat"

emptySBM :: SerializedBucketManager
emptySBM = SerializedBucketManager []

fullSBM :: SerializedBucketManager
fullSBM = SerializedBucketManager [(cns, [feat, wat])]

multiSBM :: SerializedBucketManager
multiSBM = SerializedBucketManager [(cns, [feat]), (bob, [wat, feat])]
