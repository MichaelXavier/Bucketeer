{-# LANGUAGE OverloadedStrings #-}
module Bucketeer.Testing.Types (specs) where

import Bucketeer.Testing.TestHelpers
import Bucketeer.Types

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
  where bktJSON = "{\"restore_rate\":1,\"capacity\":2,\"feature\":\"barrel_roll\",\"consumer\":\"summer\"}"


describe_Bucket_fromJSON :: Specs
describe_Bucket_fromJSON = describe "Bucketeer.Types.Bucket fromJSON" [
    it "parses well-formed json" $
      decodeJSON bktJSON ~?= Right bkt,
    it "fails json missing keys" $
      decodeJSON "{\"restore_rate\":1}" ~?= (Left "key \"consumer\" not present" :: Either String Bucket),
    it "fails json of the wrong type" $
      decodeJSON "[]" ~?= (Left "when expecting a Object, encountered Array instead" :: Either String Bucket)
  ]
  where bktJSON = "{\"restore_rate\":1,\"capacity\":2,\"feature\":\"barrel_roll\",\"consumer\":\"summer\"}"

---- Helpers
cns :: Consumer
cns = Consumer "summer"

feat :: Feature
feat = Feature "barrel_roll"

bkt = Bucket { consumer    = cns,
               feature     = feat, 
               capacity    = 2, 
               restoreRate = 1 }
