{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Bucketeer.Testing.WebServer.Util (specs) where

import Bucketeer.Types
import Bucketeer.WebServer.Util
import Bucketeer.Testing.TestHelpers (toJSONText)

import Data.String.QQ (s)
import Data.ByteString (ByteString(..))
import Data.Text.Lazy (isInfixOf)
import Test.Hspec (Specs,
                   describe,
                   descriptions,
                   it)
import Test.Hspec.HUnit
import Test.HUnit.Base ((~?=))

specs :: Specs
specs = descriptions [describe_ResponseError_toJSON,
                      describe_RemainingResponse_toJSON,
                      describe_exhaustedResponse]

describe_ResponseError_toJSON :: Specs
describe_ResponseError_toJSON = describe "Bucketeer.WebServer.Util.ResponseError toJSON" [
    it "includes the id" $ isInfixOf [s|"id":"Out of Cheese"|] text ~?= True,
    it "includes the id" $ isInfixOf [s|"description":"Redo from start"|] text ~?= True
  ]
  where text = toJSONText re
        re = ResponseError { errorId          = "Out of Cheese",
                             errorDescription = "Redo from start"}

describe_RemainingResponse_toJSON :: Specs
describe_RemainingResponse_toJSON = describe "Bucketeer.WebServer.Util.RemainingResponse toJSON" [
    it "formats the JSON properly" $ toJSONText rr ~?= [s|{"remaining":3}|]
  ]
  where rr = RemainingResponse 3

describe_exhaustedResponse :: Specs
describe_exhaustedResponse = describe "Bucketeer.WebServer.Util.exhaustedResponse" [
    it "incorportates the consumer and feature" $ exhaustedResponse cns feat ~?= resp
  ]
  where resp = ResponseError { errorId          = "Bucket Exhausted",
                               errorDescription = "barrel_roll bucket has been exhausted for summer"}

---- Helpers
cns :: Consumer
cns = Consumer "summer"

feat :: Feature
feat = Feature "barrel_roll"
