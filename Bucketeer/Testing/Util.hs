{-# LANGUAGE OverloadedStrings #-}
module Bucketeer.Testing.Util (specs) where

import Bucketeer.Types
import Bucketeer.Util

import Test.Hspec (Specs,
                   describe,
                   descriptions,
                   it)
import Test.Hspec.HUnit
import Test.HUnit.Base ((~?=))

specs :: Specs
specs = descriptions [describe_applyList,
                      describe_toMaybe]

describe_applyList :: Specs
describe_applyList = describe "Bucketeer.Util.applyList" [
    it "returns an empty list on an empty list" $ applyList ([] :: [(Char -> Char)]) 'a' ~?= [],
    it "applies each function in the list"      $ applyList [(+1), (+4), (*2)] 3 ~?= [4, 7, 6]
  ]

describe_toMaybe :: Specs
describe_toMaybe = describe "Bucketeer.Util.toMaybe" [
    it "returns Nothing if predicate fails"         $ toMaybe (==1) 2 ~?= Nothing,
    it "returns a Just value if predicate succeeds" $ toMaybe (>1) 2 ~?= Just 2
  ]
