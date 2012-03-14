{-# LANGUAGE OverloadedStrings #-}
module Bucketeer.Testing.WebServer (specs) where

import Control.Applicative ((<$>), (<*>), pure)
import Database.Redis (connect,
                       defaultConnectInfo)

import Bucketeer.Manager (startBucketManager)
import Bucketeer.Types
import Bucketeer.WebServer (BucketeerWeb(..))

import Data.IORef (newIORef)
import Database.Redis (Connection)
import Network.Wai (Application,
                    Request(..))
import Network.Wai.Test
import Test.Hspec.Monadic (Specs,
                           describe,
                           pending,
                           descriptions,
                           it)
import Test.Hspec.HUnit
import Yesod (toWaiApp)

specs :: Application
         -> Specs
specs app = do
  let webApp = flip runSession $ app
  describe "GET request to a bogus endpoint" $ do
    it "returns a 404" $ webApp $
      assertStatus 404 =<< request defaultRequest { rawPathInfo = "/sammiches" }

  describe "DELETE request to consumer" $ do
    it "non-existant consumer: returns a 404" $ webApp $
      assertStatus 404 =<< request defaultRequest { rawPathInfo = "/consumers/bogus" }
    it "returns a 204" $ 
      pending "implementation"


  describe "GET to bucket" $ do
    it "non-existant bucket: returns a 404" $ 
      pending "implementation"
    it "non-existant bucket: returns an error message" $ 
      pending "implementation"

    it "existing bucket: returns a 200" $ 
      pending "implementation"
    it "existing bucket: renders the bucket" $ 
      pending "implementation"

  describe "POST to bucket" $ do
    it "non-existant consumer: returns a 404" $ 
      pending "implementation"
    it "non-existant consumer: returns an error message" $ 
      pending "implementation"

    it "existing consumer, all params: returns a 201" $ 
      pending "implementation"
    it "existing bucket, all params: renders the bucket" $ 
      pending "implementation"

    it "existing consumer, missing capacity: returns a 400" $ 
      pending "implementation"
    it "existing consumer, missing capacity: returns an error" $ 
      pending "implementation"

    it "existing consumer, missing restore_rate: returns a 400" $ 
      pending "implementation"
    it "existing consumer, missing restore_rate: returns an error" $ 
      pending "implementation"

    it "existing consumer, missing both: returns a 400" $ 
      pending "implementation"
    it "existing consumer, missing both: returns an error" $ 
      pending "implementation"


  describe "DELETE to bucket" $ do
    it "non-existant consumer: returns a 404" $ 
      pending "implementation"
    it "non-existant consumer: returns an error message" $ 
      pending "implementation"

    it "non-existant bucket: returns a 404" $ 
      pending "implementation"
    it "non-existant bucket: returns an error message" $ 
      pending "implementation"

    it "existing bucket: returns 204" $ 
      pending "implementation"
    it "existing bucket: removes the bucket from the manager" $ 
      pending "implementation"
    it "existing bucket: revokes the bucket in redis" $ 
      pending "implementation"

  describe "POST to bucket tick" $ do
    it "non-existant consumer: returns a 404" $ 
      pending "implementation"
    it "non-existant consumer: returns an error message" $ 
      pending "implementation"

    it "non-existant bucket: returns a 404" $ 
      pending "implementation"
    it "non-existant bucket: returns an error message" $ 
      pending "implementation"

    it "existing bucket: returns 204" $ 
      pending "implementation"
    it "existing bucket: ticks the bucket" $ 
      pending "implementation"

  describe "POST to bucket refill" $ do
    it "non-existant consumer: returns a 404" $ 
      pending "implementation"
    it "non-existant consumer: returns an error message" $ 
      pending "implementation"

    it "non-existant bucket: returns a 404" $ 
      pending "implementation"
    it "non-existant bucket: returns an error message" $ 
      pending "implementation"

    it "existing bucket: returns 204" $ 
      pending "implementation"
    it "existing bucket: refills the bucket" $ 
      pending "implementation"

  describe "POST to bucket drain" $ do
    it "non-existant consumer: returns a 404" $ 
      pending "implementation"
    it "non-existant consumer: returns an error message" $ 
      pending "implementation"

    it "non-existant bucket: returns a 404" $ 
      pending "implementation"
    it "non-existant bucket: returns an error message" $ 
      pending "implementation"

    it "existing bucket: returns 204" $ 
      pending "implementation"
    it "existing bucket: drains the bucket" $ 
      pending "implementation"
