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
                          revokeFeature,
                          revokeConsumer,
                          startBucketManager,
                          storeBucketManager,
                          restoreBuckets)
import Bucketeer.Types
import Bucketeer.WebServer.Util

import Control.Applicative ((<$>))
import Control.Exception (finally)
import Control.Concurrent (forkIO,
                           killThread)
import Control.Monad.Reader (local)
import Data.ByteString (ByteString(..))
import Data.IORef (newIORef,
                   readIORef,
                   IORef,
                   atomicModifyIORef)
import Data.Maybe (isJust)
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
import Yesod.Handler
import Yesod.Request

data BucketeerWeb = BucketeerWeb { connection    :: Connection,
                                   bucketManager :: IORef BucketManager }

instance Yesod BucketeerWeb where

mkYesod "BucketeerWeb" [parseRoutes|
  /consumers/#Consumer                         ConsumerR     DELETE
  /consumers/#Consumer/buckets/#Feature        BucketR       GET POST DELETE
  /consumers/#Consumer/buckets/#Feature/tick   BucketTickR   POST
  /consumers/#Consumer/buckets/#Feature/refill BucketRefillR POST
  /consumers/#Consumer/buckets/#Feature/drain  BucketDrainR  POST
|]

main :: IO ()
main = do conn    <- connect defaultConnectInfo
          buckets <- either (exit) (return . id) =<< (runRedis conn $ restoreBuckets)
          bmRef   <- newIORef =<< startBucketManager buckets conn
          let foundation = BucketeerWeb conn bmRef
          (run 3000 =<< toWaiApp foundation) `finally` (cleanup foundation)
  where exit str = hPutStrLn stderr str >> exitFailure

---- Routes

cleanup :: BucketeerWeb
           -> IO ()
cleanup BucketeerWeb { connection    = conn,
                       bucketManager = bmRef } = do bm <- readIORef bmRef
                                                    runRedis conn $ storeBucketManager bm

--TODO: this should probably return a 404 if not found
getBucketR :: Consumer
              -> Feature
              -> Handler RepJson
getBucketR cns feat = jsonToRepJson . RemainingResponse =<< doRemaining =<< getConn
  where doRemaining conn = liftIO $ runRedis conn $ remaining cns feat 

postBucketR :: Consumer
               -> Feature
               -> Handler ()
postBucketR cns feat = do cap  <- lookupPostParam "capacity"
                          rate <- lookupPostParam "restore_rate"
                          if (isJust cap && isJust rate) then sendResponseCreated route
                          else                                invalidArgs ["capacity", "restore_rate"]
  where route = BucketR cns feat

deleteBucketR :: Consumer
                 -> Feature
                 -> Handler ()
deleteBucketR cns feat = liftIO . revoke =<< getBM
  where revoke bmRef = atomicModifyIORef bmRef (revokeFeature cns feat) >>= maybeKill
        maybeKill (Just tid) = killThread tid
        maybeKill Nothing    = return ()

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

deleteConsumerR  :: Consumer
                    -> Handler ()
deleteConsumerR cns = liftIO . revoke =<< getBM
  where revoke bmRef = atomicModifyIORef bmRef (revokeConsumer cns) >>= mapM_ (forkIO . killThread)

---- Helpers
getConn = return . connection    =<< getYesod

getBM   = return . bucketManager =<< getYesod
