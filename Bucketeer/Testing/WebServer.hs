{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Bucketeer.Testing.WebServer (runSpecs) where

import Bucketeer.Manager (startBucketManager,
                          consumerExists,
                          featureExists,
                          BucketInterface(..),
                          BucketManager)
import Bucketeer.Persistence (remaining)
import Bucketeer.Types
import Bucketeer.WebServer (BucketeerWeb(..))

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (newIORef,
                   IORef,
                   readIORef,
                   writeIORef,
                   modifyIORef)
import Database.Redis (Connection,
                       hset,
                       runRedis)
import Network.HTTP.Types (methodPost,
                           methodGet,
                           methodDelete,
                           methodPut,
                           headerAccept,
                           renderSimpleQuery,
                           simpleQueryToQuery)
import Network.Wai (Application,
                    Request(..))
import Network.Wai.Test
import Test.Hspec.HUnit
import Test.HUnit (assertBool)
import Data.String.QQ (s)
import Yesod (toWaiApp)
import Yesod.Test
import qualified Control.Monad.Trans.State as ST


---DEBUG includes
import Control.Concurrent (forkIO,
                           ThreadId)
import qualified Data.HashMap.Strict as H

runSpecs :: Connection
            -> IO ()
runSpecs conn = do bmRef <- newIORef =<< newBM
                   app   <- toWaiApp $ BucketeerWeb conn bmRef
                   runTests app undefined $ specs conn bmRef

specs :: Connection
         -> IORef BucketManager
         -> Specs
specs conn bmRef = do
  let beforeRun = liftIO $ do resetBMRef bmRef
                              resetCount conn 1
  describe "GET request to a bogus endpoint" $ do
    it "returns a 404" $ beforeRun >> do
      get_ "bogus"
      statusIs 404

  --- DELETE /consumers/#Consumer
  describe "DELETE to non-existant consumer" $ do
    it "returns a 404" $ beforeRun >> do
      delete_ "consumers/bogus"

      statusIs 404
      -- Not enough API exposed in test to write a more precise matcher
      bodyContains [s|"description":"Could not find consumer bogus"|]
      bodyContains [s|"id":"Consumer Not Found"|]

  describe "DELETE request to existing consumer" $ do
    it "returns a 204, deleting the consumer" $ beforeRun >> do
      delete_ "consumers/summer"

      statusIs 204

      bm <- liftIO $ readIORef bmRef

      assertEqual "Consumer removed" False $ consumerExists cns bm
      assertRemaining conn 0

  --- GET /consumers/#Consumer/buckets/#Feature
  describe "GET to non-existant bucket" $ do
    it "returns a 404" $ beforeRun >> do
      get_ "consumers/summer/buckets/bogus"

      statusIs 404

      bodyContains [s|"description":"Could not find feature (summer, bogus)"|]
      bodyContains [s|"id":"Feature Not Found"|]


  describe "GET to existing bucket" $ do
    it "returns a 200" $ beforeRun >> do
      get_ "consumers/summer/buckets/barrel_roll"

      statusIs 200

      bodyContains [s|{"remaining":1}|]


  --- POST /consumers/#Consumer/buckets
  describe "POST to non-existant consumer" $ do
    it "returns a 404" $ beforeRun >> do
      post_ "consumers/bogus/buckets/bogus"

      statusIs 404

      bodyContains [s|"description":"Could not find consumer bogus"|]
      bodyContains [s|"id":"Consumer Not Found"|]


  describe "POST to existing consumer, all params" $ do
    let params = postParams [("capacity", "10"), ("restore_rate", "9000")]

    it "returns a 201, filling the user" $ beforeRun >> do
      post "consumers/summer/buckets/barrel_roll" $ params

      statusIs 201
      --TODO: verify Location header

      assertRemaining conn 10


  describe "POST to existing consumer, missing capacity" $ do
    let params = postParams [("restore_rate", "9000")]

    it "returns a 400" $ beforeRun >> do
      post "consumers/summer/buckets/barrel_roll" $ params

      statusIs 400

      bodyContains [s|[{"description":"capacity and restore_rate params required","id":"Missing Parameters"}]|]


  describe "POST to existing consumer, missing both" $ do
    let params = postParams [("capacity", "10")]

    it "returns a 400" $ beforeRun >> do
      post "consumers/summer/buckets/barrel_roll" $ params

      statusIs 400

      bodyContains [s|[{"description":"capacity and restore_rate params required","id":"Missing Parameters"}]|]


  --- DELETE /consumers/#Consumer/buckets/#Bucket
  describe "DELETE to non-existant bucket" $ do
    it "returns a 404" $ beforeRun >> do
      delete_ "consumers/summer/buckets/bogus"

      statusIs 404

      bodyContains [s|"description":"Could not find feature (summer, bogus)"|]
      bodyContains [s|"id":"Feature Not Found"|]


  describe "DELETE to an existing bucket" $ do
    it "returns a 204" $ beforeRun >> do
      delete_ "consumers/summer/buckets/barrel_roll"

      statusIs 204

      bm <- liftIO $ readIORef bmRef

      assertEqual "Bucket removed" False $ featureExists cns feat bm

      assertRemaining conn 0

      --TODO: assert with redis

  --- POST /consumers/#Consumer/buckets/#Bucket/tick
  describe "POST to non-existant bucket tick" $ do
    it "returns a 404" $ beforeRun >> do
      post_ "consumers/summer/buckets/bogus/tick"

      statusIs 404

      bodyContains [s|"description":"Could not find feature (summer, bogus)"|]
      bodyContains [s|"id":"Feature Not Found"|]


  describe "POST to existing bucket tick" $ do
    it "returns 200" $ beforeRun >> do
      post_ "consumers/summer/buckets/barrel_roll/tick"

      statusIs 200

      bodyContains [s|{"remaining":0}|]
      assertRemaining conn 0

  --- POST /consumers/#Consumer/buckets/#Bucket/refill
  describe "POST to non-existant bucket refill" $ do
    it "returns a 404" $ beforeRun >> do
      post_ "consumers/summer/buckets/bogus/refill"

      statusIs 404

      bodyContains [s|"description":"Could not find feature (summer, bogus)"|]
      bodyContains [s|"id":"Feature Not Found"|]

  describe "POST to existing bucket refill" $ do
    it "returns 200" $ beforeRun >> do
      post_ "consumers/summer/buckets/barrel_roll/refill"

      statusIs 200
      --TODO check remaining in body, check redis

  --- POST /consumers/#Consumer/buckets/#Bucket/drain
  describe "POST to non-existant bucket drain" $ do
    it "returns a 404" $ beforeRun >> do
      post_ "consumers/summer/buckets/bogus/drain"

      statusIs 404

      bodyContains [s|"description":"Could not find feature (summer, bogus)"|]
      bodyContains [s|"id":"Feature Not Found"|]

  describe "POST to existing bucket drain" $ do
    it "returns 200" $ beforeRun >> do
      post_ "consumers/summer/buckets/barrel_roll/drain"

      statusIs 200
      --TODO check remaining in body, check redis



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

postParams pairs = mapM_ (uncurry byName ) pairs

delete_ :: BS8.ByteString -> OneSpec ()
delete_ url = doRequest "DELETE" url $ return ()

newBM :: IO BucketManager
newBM = return . fullBM =<< dummyTid

resetBMRef :: IORef (BucketManager)
              -> IO ()
resetBMRef bmRef = (writeIORef bmRef =<< newBM)

assertRemaining conn n = do actual <- liftIO $ runRedis conn $ remaining cns feat
                            assertEqual (msg actual) n actual
  where msg actual = "Remaining count expected " ++ show n ++ ", was " ++ show actual

resetCount :: Connection
              -> Integer
              -> IO ()
resetCount conn n = liftIO $ runRedis conn $ hset "bucketeer:buckets:summer" "barrel_roll" (bsN) >> return ()
  where bsN = BS8.pack . show $ n
