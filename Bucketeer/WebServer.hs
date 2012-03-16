{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Bucketeer.WebServer (main,
                            BucketeerWeb(..)) where

import Bucketeer.Persistence (remaining,
                              tick,
                              drain,
                              refill,
                              TickResult(..))
import Bucketeer.Manager (BucketManager,
                          featureExists,
                          consumerExists,
                          revokeFeature,
                          revokeConsumer,
                          startBucketManager,
                          storeBucketManager,
                          restoreBuckets)
import Bucketeer.Types
import Bucketeer.WebServer.Util

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO,
                           killThread)
import Control.Exception (finally)
import Control.Monad (when)
import Data.Aeson (toJSON)
import Data.ByteString (ByteString(..))
import Data.IORef (newIORef,
                   readIORef,
                   IORef,
                   atomicModifyIORef)
import Data.Maybe (isJust)
import Data.Text (Text(..))
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T
import Database.Redis (Connection,
                       connect,
                       runRedis,
                       defaultConnectInfo)
import Network.HTTP.Types (Status,
                           notFound404)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Exit (exitFailure)
import System.IO (hPutStrLn,
                  stderr)
import Yesod
import Yesod.Handler
import Yesod.Request

----DEBUG IMPORTS
import qualified Data.HashMap.Strict as H
import Data.Text (pack)

data BucketeerWeb = BucketeerWeb { connection    :: Connection,
                                   bucketManager :: IORef BucketManager }

instance Yesod BucketeerWeb where
  approot = ApprootRelative
  logLevel _ = LevelDebug

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
          app <- toWaiApp foundation
          (run 3000 $ logWare app) `finally` (cleanup foundation)
  where exit str = hPutStrLn stderr str >> exitFailure

logWare :: Middleware
logWare = logStdoutDev

---- Routes

cleanup :: BucketeerWeb
           -> IO ()
cleanup BucketeerWeb { connection    = conn,
                       bucketManager = bmRef } = do bm <- readIORef bmRef
                                                    runRedis conn $ storeBucketManager bm

getBucketR :: Consumer
              -> Feature
              -> Handler RepJson
getBucketR cns feat = checkFeature cns feat $ jsonToRepJson . RemainingResponse =<< doRemaining =<< getConn
  where doRemaining conn = liftIO $ runRedis conn $ remaining cns feat 

postBucketR :: Consumer
               -> Feature
               -> Handler ()
postBucketR cns feat = checkConsumer cns $ do cap  <- lookupPostParam "capacity"
                                              rate <- lookupPostParam "restore_rate"
                                              if (isJust cap && isJust rate) then sendResponseCreated route
                                              else                                invalidArgs ["capacity", "restore_rate"]
  where route = BucketR cns feat

deleteBucketR :: Consumer
                 -> Feature
                 -> Handler ()
deleteBucketR cns feat = checkFeature cns feat $ liftIO . revoke =<< getBM
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
deleteConsumerR cns = checkConsumer cns $ liftIO . revoke =<< getBM
  where revoke bmRef = atomicModifyIORef bmRef (revokeConsumer cns) >>= mapM_ (forkIO . killThread)

---- Helpers
getConn = return . connection    =<< getYesod

getBM   = return . bucketManager =<< getYesod

readBM = liftIO . readIORef =<< getBM

checkConsumer cns@(Consumer c) inner = switch =<< readBM
  where switch bm = if check bm then notFound else inner
        check     = not . consumerExists cns
        notFound  = sendError notFound404 [("Consumer Not Found", T.concat ["Could not find consumer ", b2t c])]

checkFeature cns@(Consumer c)
             feat@(Feature f) inner = do switch =<< readBM
  where switch bm = if check bm then notFound else inner
        check     = not . featureExists cns feat
        notFound  = sendError notFound404 [("Feature Not Found", T.concat ["Could not find feature (",
                                                                          b2t c,
                                                                          ", ",
                                                                          b2t f,
                                                                          ")" ])]

b2t :: ByteString
       -> Text
b2t = decodeUtf8
  
sendError :: Status
             -> [(Text, Text)]
             -> GHandler s m a
sendError status errs = sendResponseStatus status repErrs
  where repErrs        = RepJson . toContent . toJSON $ responseErrors
        responseErrors = map (uncurry ResponseError) errs
