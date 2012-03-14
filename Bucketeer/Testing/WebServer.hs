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
import Network.HTTP.Types (methodPost,
                           methodGet,
                           methodDelete,
                           methodPut)
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
      assertStatus 404 =<< request defaultRequest { rawPathInfo   = "/sammiches",
                                                    requestMethod = methodGet}

  --- DELETE /consumers/#Consumer
  describe "DELETE to non-existant consumer" $ do
    let req = defaultRequest { rawPathInfo = "/consumers/bogus" }
    it "non-existant consumer: returns a 404" $ webApp $
      assertStatus 404 =<< request req

  describe "DELETE request to existing consumer" $ do
    it "returns a 204" $ 
      pending "implementation"


  --- GET /consumers/#Consumer/buckets/#Feature
  describe "GET to non-existant bucket" $ do
    let req = defaultRequest { rawPathInfo   = "/consumers/summer/buckets/bogus",
                               requestMethod = methodGet}

    it "returns a 404" $ webApp $
      assertStatus 404 =<< request req
    it "returns an error message" $ webApp $
      assertBody "TODO" =<< request req --FIXME

  describe "GET to existing bucket" $ do
    it "returns a 200" $ 
      pending "implementation"
    it "renders the bucket" $ 
      pending "implementation"


  --- POST /consumers/#Consumer/buckets
  describe "POST to non-existant consumer" $ do
    let req = defaultRequest { rawPathInfo   = "/consumers/summer/buckets/bogus",
                               requestMethod = methodPost }
    let sreq = SRequest req ""

    it "returns a 404" $ webApp $
      assertStatus 404 =<< srequest sreq

    it "returns an error message" $ webApp $
      assertBody "TODO" =<< srequest sreq --FIXME

  describe "POST to existing consumer, all params" $ do
    it "returns a 201" $ 
      pending "implementation"
    it "renders the bucket" $ 
      pending "implementation"

  describe "POST to existing consumer, missing capacity" $ do
    it "returns a 400" $ 
      pending "implementation"
    it "returns an error" $ 
      pending "implementation"

  describe "POST to existing consumer, missing restore_rate" $ do
    it "returns a 400" $ 
      pending "implementation"
    it "returns an error" $ 
      pending "implementation"

  describe "POST to existing consumer, missing both" $ do
    it "returns a 400" $ 
      pending "implementation"
    it "returns an error" $ 
      pending "implementation"


  --- DELETE /consumers/#Consumer/buckets/#Bucket
  describe "DELETE to non-existant bucket" $ do
    let req = defaultRequest { rawPathInfo   = "/consumers/summer/buckets/bogus",
                               requestMethod = methodDelete }

    it "returns a 404" $ webApp $
      assertStatus 404 =<< request req

    it "returns an error message" $ webApp $
      assertBody "TODO" =<< request req --FIXME

  describe "DELETE to existing bucket" $ do
    it "returns 204" $ 
      pending "implementation"
    it "removes the bucket from the manager" $ 
      pending "implementation"
    it "revokes the bucket in redis" $ 
      pending "implementation"

  --- POST /consumers/#Consumer/buckets/#Bucket/tick
  describe "POST to non-existant bucket tick" $ do
    let req = defaultRequest { rawPathInfo   = "/consumers/summer/buckets/bogus/tick",
                               requestMethod = methodPost }

    it "returns a 404" $ webApp $
      assertStatus 404 =<< request req

    it "returns an error message" $ webApp $
      assertBody "TODO" =<< request req --FIXME


  describe "POST to existing bucket tick" $ do
    it "returns 204" $ 
      pending "implementation"
    it "ticks the bucket" $ 
      pending "implementation"


  --- POST /consumers/#Consumer/buckets/#Bucket/refill
  describe "POST to non-existant bucket refill" $ do
    let req = defaultRequest { rawPathInfo   = "/consumers/summer/buckets/bogus/refill",
                               requestMethod = methodPost }

    it "returns a 404" $ webApp $
      assertStatus 404 =<< request req

    it "returns an error message" $ webApp $
      assertBody "TODO" =<< request req --FIXME


  describe "POST to existing bucket refill" $ do
    it "returns 204" $ 
      pending "implementation"
    it "refills the bucket" $ 
      pending "implementation"


  --- POST /consumers/#Consumer/buckets/#Bucket/drain
  describe "POST to non-existant bucket drain" $ do
    let req = defaultRequest { rawPathInfo   = "/consumers/summer/buckets/bogus/drain",
                               requestMethod = methodPost }

    it "returns a 404" $ webApp $
      assertStatus 404 =<< request req

    it "returns an error message" $ webApp $
      assertBody "TODO" =<< request req --FIXME


  describe "POST to existing bucket drain" $ do
    it "returns 204" $ 
      pending "implementation"
    it "drains the bucket" $ 
      pending "implementation"
