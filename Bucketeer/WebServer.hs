{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Bucketeer.WebServer (main) where

import Bucketeer.Persistence (remaining,
                              tick,
                              drain,
                              refill,
                              TickResult(..))
import Bucketeer.Manager (BucketManager,
                          startBucketManager,
                          restoreBuckets)
import Bucketeer.Types
import Bucketeer.WebServer.Util

import Data.ByteString (ByteString(..))
import Data.Text (Text(..))
import Database.Redis (Connection,
                       connect,
                       runRedis,
                       defaultConnectInfo)
import Network.Wai.Handler.Warp (run)
import System.Exit (exitFailure)
import System.IO (hPutStrLn,
                  stderr)
import Yesod

data BucketeerWeb = BucketeerWeb { connection    :: Connection,
                                   bucketManager :: BucketManager }

instance Yesod BucketeerWeb where


--TODO: deletion of consumers, buckets?

mkYesod "BucketeerWeb" [parseRoutes|
  /consumers/#Consumer/buckets/#Feature        BucketR       GET POST DELETE
  /consumers/#Consumer/buckets/#Feature/tick   BucketTickR   POST
  /consumers/#Consumer/buckets/#Feature/refill BucketRefillR POST
  /consumers/#Consumer/buckets/#Feature/drain  BucketDrainR  POST
|]

getBucketR :: Consumer
              -> Feature
              -> Handler RepJson
getBucketR cns feat = jsonToRepJson . RemainingResponse =<< doRemaining =<< getConn
  where doRemaining conn = liftIO $ runRedis conn $ remaining cns feat 

--TODO: need manager
postBucketR :: Consumer
               -> Feature
               -> Handler ()
postBucketR cns feat = undefined 

deleteBucketR :: Consumer
                 -> Feature
                 -> Handler ()
deleteBucketR cns feat = undefined

--TODO: handle throttle with statuscode
postBucketTickR :: Consumer
                   -> Feature
                   -> Handler RepJson
postBucketTickR cns feat = repResponse . tickResponse cns feat =<< doTick =<< getConn
  where doTick conn = liftIO $ runRedis conn $ tick cns feat
        repResponse = either jsonToRepJson jsonToRepJson 

postBucketRefillR :: Consumer
                     -> Feature
                     -> Handler ()
postBucketRefillR cns feat = doRefill =<< getConn
  where doRefill conn = liftIO $ runRedis conn $ refill cns feat 15

postBucketDrainR :: Consumer
                    -> Feature
                    -> Handler ()
postBucketDrainR cns feat = doDrain =<< getConn
  where doDrain conn = liftIO $ runRedis conn $ drain cns feat

main :: IO ()
main = do conn <- connect defaultConnectInfo
          buckets <- either (exit) (return . id) =<< (runRedis conn $ restoreBuckets)
          bm   <- startBucketManager buckets conn
          run 3000 =<< toWaiApp (BucketeerWeb conn bm)
  where exit str = hPutStrLn stderr str >> exitFailure

---- Helpers
getConn = return . connection =<< getYesod
