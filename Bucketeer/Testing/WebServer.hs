{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Bucketeer.Testing.WebServer (specs) where

import Bucketeer.Manager (startBucketManager)
import Bucketeer.Types
import Bucketeer.WebServer (BucketeerWeb(..))

import Data.ByteString (ByteString)
import Data.IORef (newIORef)
import Database.Redis (Connection)
import Database.Redis (connect,
                       defaultConnectInfo)
import Network.HTTP.Types (methodPost,
                           methodGet,
                           methodDelete,
                           methodPut,
                           headerAccept)
import Network.Wai (Application,
                    Request(..))
import Network.Wai.Test
import Test.Hspec.Monadic (Specs,
                           describe,
                           pending,
                           descriptions,
                           it)
import Test.Hspec.HUnit
import Test.HUnit.Base
import Data.String.QQ (s)
import Yesod (toWaiApp)

specs :: Application
         -> Specs
specs app = do
  let webApp = flip runSession $ app
  describe "GET request to a bogus endpoint" $ do
    let path = "bogus"

    it "returns a 404" $ webApp $
      assertStatus 404 =<< request (setRawPathInfo getRequest path)

  --- DELETE /consumers/#Consumer
  describe "DELETE to non-existant consumer" $ do
    let path = "consumers/bogus"

    it "non-existant consumer: returns a 404" $ webApp $
      assertStatus 404 =<< request (setRawPathInfo deleteRequest path)

  describe "DELETE request to existing consumer" $ do
    let path = "consumers/summer"

    it "returns a 204" $ 
      pending "implementation"


  --- GET /consumers/#Consumer/buckets/#Feature
  describe "GET to non-existant bucket" $ do
    let path = "consumers/summer/buckets/bogus"

    it "returns a 404" $ webApp $
      assertStatus 404 =<< request (setRawPathInfo getRequest path)
    it "returns an error message" $ webApp $
      assertBody [s|[{"description":"Could not find feature (summer, bogus)","id":"Feature Not Found"}]|] =<< request (setRawPathInfo getRequest path)

  describe "GET to existing bucket" $ do
    let path = "consumers/summer/buckets/barrel_roll"

    it "returns a 200" $ 
      pending "implementation"
    it "renders the bucket" $ 
      pending "implementation"


  --- POST /consumers/#Consumer/buckets
  describe "POST to non-existant consumer" $ do
    let path = "consumers/bogus/buckets/barrel_roll"
    let sreq = SRequest (setRawPathInfo postRequest path) ""

    it "returns a 404" $ webApp $
      assertStatus 404 =<< srequest sreq

    it "returns an error message" $ webApp $
      assertBody [s|[{"description":"Could not find consumer bogus","id":"Consumer Not Found"}]|] =<< srequest sreq
--
--  describe "POST to existing consumer, all params" $ do
    -- let path = "consumers/summer/buckets/barrel_roll"

--    it "returns a 201" $ 
--      pending "implementation"
--    it "renders the bucket" $ 
--      pending "implementation"
--
--  describe "POST to existing consumer, missing capacity" $ do
--    it "returns a 400" $ 
--      pending "implementation"
--    it "returns an error" $ 
--      pending "implementation"
--
--  describe "POST to existing consumer, missing restore_rate" $ do
--    it "returns a 400" $ 
--      pending "implementation"
--    it "returns an error" $ 
--      pending "implementation"
--
--  describe "POST to existing consumer, missing both" $ do
--    it "returns a 400" $ 
--      pending "implementation"
--    it "returns an error" $ 
--      pending "implementation"
--
--
  --- DELETE /consumers/#Consumer/buckets/#Bucket
  describe "DELETE to non-existant bucket" $ do
    let path = "consumers/summer/buckets/bogus"

    it "returns a 404" $ webApp $
      assertStatus 404 =<< request (setRawPathInfo deleteRequest path)

    it "returns an error message" $ webApp $
      assertBody [s|[{"description":"Could not find feature (summer, bogus)","id":"Feature Not Found"}]|] =<< request (setRawPathInfo deleteRequest path)
--
--  describe "DELETE to existing bucket" $ do
--    it "returns 204" $ 
--      pending "implementation"
--    it "removes the bucket from the manager" $ 
--      pending "implementation"
--    it "revokes the bucket in redis" $ 
--      pending "implementation"
--
  --- POST /consumers/#Consumer/buckets/#Bucket/tick
  describe "POST to non-existant bucket tick" $ do
    let path = "consumers/summer/buckets/bogus/tick"
    let sreq = SRequest (setRawPathInfo postRequest path) ""

    it "returns a 404" $ webApp $
      assertStatus 404 =<< srequest sreq

    it "returns an error message" $ webApp $
      assertBody [s|[{"description":"Could not find feature (summer, bogus)","id":"Feature Not Found"}]|] =<< srequest sreq
--
--
--  describe "POST to existing bucket tick" $ do
--    it "returns 204" $ 
--      pending "implementation"
--    it "ticks the bucket" $ 
--      pending "implementation"
--
--
  --- POST /consumers/#Consumer/buckets/#Bucket/refill
  describe "POST to non-existant bucket refill" $ do
    let path = "consumers/summer/buckets/bogus/refill"
    let sreq = SRequest (setRawPathInfo postRequest path) ""

    it "returns a 404" $ webApp $
      assertStatus 404 =<< srequest sreq

    it "returns an error message" $ webApp $
      assertBody [s|[{"description":"Could not find feature (summer, bogus)","id":"Feature Not Found"}]|] =<< srequest sreq
--
--
--  describe "POST to existing bucket refill" $ do
--    it "returns 204" $ 
--      pending "implementation"
--    it "refills the bucket" $ 
--      pending "implementation"
--
--
--  --- POST /consumers/#Consumer/buckets/#Bucket/drain
  describe "POST to non-existant bucket drain" $ do
    let path = "consumers/summer/buckets/bogus/drain"
    let sreq = SRequest (setRawPathInfo postRequest path) ""

    it "returns a 404" $ webApp $
      assertStatus 404 =<< srequest sreq

    it "returns an error message" $ webApp $
      assertBody [s|[{"description":"Could not find feature (summer, bogus)","id":"Feature Not Found"}]|] =<< srequest sreq
--
--
--  describe "POST to existing bucket drain" $ do
--    it "returns 204" $ 
--      pending "implementation"
--    it "drains the bucket" $ 
--      pending "implementation"

getRequest :: Request
getRequest = baseRequest { requestMethod = methodGet }

postRequest :: Request
postRequest = baseRequest { requestMethod = methodPost }
                           
deleteRequest :: Request
deleteRequest = baseRequest { requestMethod = methodDelete }

baseRequest :: Request
baseRequest = defaultRequest { requestHeaders = [headerAccept "application/json"] }
