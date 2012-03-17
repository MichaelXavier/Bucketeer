{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Bucketeer.Testing.WebServer (specs) where

import Bucketeer.Manager (startBucketManager,
                          BucketInterface(..),
                          BucketManager)
import Bucketeer.Types
import Bucketeer.WebServer (BucketeerWeb(..))

import Data.ByteString (ByteString)
import Data.IORef (newIORef,
                   IORef,
                   modifyIORef)
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


---DEBUG includes
import Control.Concurrent (forkIO,
                           ThreadId)
import qualified Data.HashMap.Strict as H

specs :: Connection
         -> Specs
specs conn = do
  describe "GET request to a bogus endpoint" $ do
    let path    = "bogus"
    let req = (setRawPathInfo getRequest path)

    it "returns a 404" $ 
      runSession (assertStatus 404 =<< request req) =<<
                  defaultApp conn

  --- DELETE /consumers/#Consumer
  describe "DELETE to non-existant consumer" $ do
    let path = "consumers/bogus"
    let req = (setRawPathInfo deleteRequest path)

    it "non-existant consumer: returns a 404" $
      runSession (assertStatus 404 =<< request req) =<<
                  defaultApp conn

  describe "DELETE request to existing consumer" $ do
    let path = "consumers/summer"

    it "returns a 204" $ 
      pending "implementation"


  --- GET /consumers/#Consumer/buckets/#Feature
  describe "GET to non-existant bucket" $ do
    let path = "consumers/summer/buckets/bogus"
    let req = (setRawPathInfo deleteRequest path)

    it "returns a 404" $
      runSession (assertStatus 404 =<< request req) =<<
                  defaultApp conn
    it "returns an error message" $
      runSession (assertBody [s|[{"description":"Could not find feature (summer, bogus)","id":"Feature Not Found"}]|] =<< request req) =<<
                  defaultApp conn

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

    it "returns a 404" $
      runSession (assertStatus 404 =<< srequest sreq) =<<
                  defaultApp conn

    it "returns an error message" $
      runSession (assertBody [s|[{"description":"Could not find consumer bogus","id":"Consumer Not Found"}]|] =<< srequest sreq) =<<
                  defaultApp conn

  describe "POST to existing consumer, all params" $ do
    let path = "consumers/summer/buckets/barrel_roll"

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
    let path = "consumers/summer/buckets/bogus"
    let req = (setRawPathInfo deleteRequest path)

    it "returns a 404" $
      runSession (assertStatus 404 =<< request req) =<<
                  defaultApp conn

    it "returns an error message" $
      runSession (assertBody [s|[{"description":"Could not find feature (summer, bogus)","id":"Feature Not Found"}]|] =<< request req) =<<
                 defaultApp conn

  describe "DELETE to existing bucket" $ do
    it "returns 204" $ 
      pending "implementation"
    it "removes the bucket from the manager" $ 
      pending "implementation"
    it "revokes the bucket in redis" $ 
      pending "implementation"

  --- POST /consumers/#Consumer/buckets/#Bucket/tick
  describe "POST to non-existant bucket tick" $ do
    let path = "consumers/summer/buckets/bogus/tick"
    let sreq = SRequest (setRawPathInfo postRequest path) ""

    it "returns a 404" $
      runSession (assertStatus 404 =<< srequest sreq) =<<
                  defaultApp conn

    it "returns an error message" $
      runSession (assertBody [s|[{"description":"Could not find feature (summer, bogus)","id":"Feature Not Found"}]|] =<< srequest sreq) =<<
                 defaultApp conn


  describe "POST to existing bucket tick" $ do
    it "returns 204" $ 
      pending "implementation"
    it "ticks the bucket" $ 
      pending "implementation"


  --- POST /consumers/#Consumer/buckets/#Bucket/refill
  describe "POST to non-existant bucket refill" $ do
    let path = "consumers/summer/buckets/bogus/refill"
    let sreq = SRequest (setRawPathInfo postRequest path) ""

    it "returns a 404" $
      runSession (assertStatus 404 =<< srequest sreq) =<<
                  defaultApp conn

    it "returns an error message" $
      runSession (assertBody [s|[{"description":"Could not find feature (summer, bogus)","id":"Feature Not Found"}]|] =<< srequest sreq) =<<
                 defaultApp conn


  describe "POST to existing bucket refill" $ do
    it "returns 204" $ 
      pending "implementation"
    it "refills the bucket" $ 
      pending "implementation"


  --- POST /consumers/#Consumer/buckets/#Bucket/drain
  describe "POST to non-existant bucket drain" $ do
    let path = "consumers/summer/buckets/bogus/drain"
    let sreq = SRequest (setRawPathInfo postRequest path) ""

    it "returns a 404" $
      runSession (assertStatus 404 =<< srequest sreq) =<<
                  defaultApp conn

    it "returns an error message" $
      runSession (assertBody [s|[{"description":"Could not find feature (summer, bogus)","id":"Feature Not Found"}]|] =<< srequest sreq) =<<
                 defaultApp conn


  describe "POST to existing bucket drain" $ do
    it "returns 204" $ 
      pending "implementation"
    it "drains the bucket" $ 
      pending "implementation"

getRequest :: Request
getRequest = baseRequest { requestMethod = methodGet }

postRequest :: Request
postRequest = baseRequest { requestMethod = methodPost }
                           
deleteRequest :: Request
deleteRequest = baseRequest { requestMethod = methodDelete }

baseRequest :: Request
baseRequest = defaultRequest { requestHeaders = [headerAccept "application/json"] }

dummyTid :: IO (ThreadId)
dummyTid = forkIO $ return ()

loadFixtures :: ThreadId
                -> IORef (BucketManager)
                -> IO ()
loadFixtures tid bmRef = modifyIORef bmRef (const $ fullBM tid)

emptyBM :: BucketManager
emptyBM = H.empty

fullBM :: ThreadId
          -> BucketManager
fullBM tid = H.singleton (cns, feat) $ bi tid

cns :: Consumer
cns = Consumer "summer"

feat :: Feature
feat = Feature "barrel_roll"

bi :: ThreadId
      -> BucketInterface
bi tid = BucketInterface { bucket         = bkt,
                           refillerThread = tid }

bkt :: Bucket
bkt = Bucket { consumer    = cns,
               feature     = feat,
               capacity    = 2,
               restoreRate = 1000 }

defaultApp :: Connection
              -> IO Application
defaultApp conn = do bmRef <- newIORef emptyBM
                     toWaiApp $ BucketeerWeb conn bmRef

loadedApp :: Connection
              -> IO Application
loadedApp conn = do bmRef <- newIORef . fullBM =<< dummyTid
                    toWaiApp $ BucketeerWeb conn bmRef

