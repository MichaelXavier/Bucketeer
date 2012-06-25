{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Bucketeer.Testing.Types (specs) where

import Data.ByteString.Lazy (fromChunks)
import Bucketeer.Testing.TestHelpers
import Bucketeer.Types

import Data.String.QQ (s)
import Test.Hspec (Specs,
                   describe,
                   descriptions,
                   pending,
                   it)
import Test.Hspec.HUnit
import Test.HUnit.Base ((~?=))

specs :: Specs
specs = descriptions [describe_Bucket_toJSON,
                      describe_Bucket_fromJSON ]

describe_Bucket_toJSON :: Specs
describe_Bucket_toJSON = describe "Bucketeer.Types.Bucket toJSON" [
    it "serializes the bucket" $ toJSONText bkt ~?= bktJSON
  ]
  where bktJSON = [s|{"restore_rate":1,"capacity":2,"feature":"barrel_roll","consumer":"summer"}|]


describe_Bucket_fromJSON :: Specs
describe_Bucket_fromJSON = describe "Bucketeer.Types.Bucket fromJSON" [
    it "parses well-formed json" $
      decodeJSON bktJSON ~?= Right bkt,
    it "fails json missing keys" $
      decodeJSON restoreRate ~?= (Left "key \"consumer\" not present" :: Either String Bucket),
    it "fails json of the wrong type" $
      decodeJSON empty ~?= (Left "when expecting a Object, encountered Array instead" :: Either String Bucket)
  ]
  where bktJSON = fromChunks [[s|{"restore_rate":1,"capacity":2,"feature":"barrel_roll","consumer":"summer"}|]]
        restoreRate = fromChunks [[s|{"restore_rate":1}|]]
        empty = fromChunks ["[]"]

---- Helpers
cns :: Consumer
cns = Consumer "summer"

feat :: Feature
feat = Feature "barrel_roll"

bkt = Bucket { consumer    = cns,
               feature     = feat, 
               capacity    = 2, 
               restoreRate = 1 }
