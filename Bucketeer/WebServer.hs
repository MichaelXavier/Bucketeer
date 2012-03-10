{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Bucketeer.WebServer (main) where

import Bucketeer.Persistence (remaining,
                              tick,
                              TickResult(..))
import Bucketeer.Types
import Bucketeer.WebServer.Util

import Data.ByteString (ByteString(..))
import Database.Redis (Connection,
                       connect,
                       runRedis,
                       defaultConnectInfo)
import Network.Wai.Handler.Warp (run)
import Data.Text (Text(..))
import Yesod

instance Yesod BucketeerWeb where

data BucketeerWeb = BucketeerWeb { connection :: Connection }


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

postBucketTickR :: Consumer
               -> Feature
               -> Handler RepJson
postBucketTickR cns feat = jsonToRepJson . tickResponse =<< doTick =<< getConn
  where doTick conn = liftIO $ runRedis conn $ tick cns feat
        exhaustedResp = RepPlain . toContent $ ("chill" :: Text)

main :: IO ()
main = do conn <- connect defaultConnectInfo
          run 3000 =<< toWaiApp (BucketeerWeb conn)


---- Helpers
getConn = return . connection =<< getYesod
