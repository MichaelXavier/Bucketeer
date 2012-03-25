{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DoAndIfThenElse       #-}
module Bucketeer.WebServer (main,
                            BucketeerWeb(..)) where

import Bucketeer.Persistence (remaining,
                              tick,
                              drain,
                              refill,
                              deleteFeature,
                              deleteConsumer)
import Bucketeer.Manager (BucketManager,
                          featureExists,
                          consumerExists,
                          replaceBucket,
                          revokeFeature,
                          revokeConsumer,
                          startBucketManager,
                          storeBucketManager,
                          runRefiller,
                          restoreBuckets,
                          BucketInterface(..))
import Bucketeer.Types
import Bucketeer.Util (forkWaitingIO,
                       (.:),
                       maybeRead)
import Bucketeer.WebServer.Util

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO,
                           ThreadId,
                           killThread)
import Control.Concurrent.MVar (putMVar)
import Control.Exception (finally)
import Control.Monad (join)
import Data.Aeson (toJSON)
import Data.ByteString (ByteString)
import Data.HashMap.Strict ((!))
import Data.IORef (newIORef,
                   readIORef,
                   IORef,
                   atomicModifyIORef)
import Data.Maybe (isJust,
                   fromJust)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T
import Database.Redis (Connection,
                       connect,
                       runRedis,
                       defaultConnectInfo)
import Network.HTTP.Types (Status,
                           noContent204,
                           status400,
                           notFound404)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Exit (exitFailure)
import System.IO (hPutStrLn,
                  stderr)
import Yesod

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
postBucketR cns feat = checkConsumer cns $ do cap    <- join <$> (maybeRead . T.unpack) .: lookupPostParam "capacity"
                                              rate   <- join <$> (maybeRead . T.unpack) .: lookupPostParam "restore_rate"
                                              bmRef  <- getBM
                                              conn   <- getConn
                                              if (isJust cap && isJust rate) then
                                                create bmRef conn $ Bucket cns feat (fromJust cap) (fromJust rate)
                                              else
                                                sendError status400 [("Missing Parameters", "capacity and restore_rate params required")]
  where create bmRef conn bkt = do liftIO $ do atomicAddFeature bmRef conn bkt
                                               runRedis conn $ refill cns feat $ capacity bkt
                                               backgroundDump conn =<< readIORef bmRef
                                   sendResponseCreated route
        route = BucketR cns feat

deleteBucketR :: Consumer
                 -> Feature
                 -> Handler ()
deleteBucketR cns feat = checkFeature cns feat $ handler >> sendNoContent
  where handler      = do bmRef <- getBM
                          conn  <- getConn
                          liftIO $ revoke bmRef conn
        revoke bmRef conn = do atomicKillFeature bmRef cns feat
                               runRedis conn $ deleteFeature cns feat
                               backgroundDump conn =<< readIORef bmRef

postBucketTickR :: Consumer
                   -> Feature
                   -> Handler RepJson
postBucketTickR cns feat = checkFeature cns feat $ repResponse . tickResponse cns feat =<< doTick =<< getConn
  where doTick conn = liftIO $ runRedis conn $ tick cns feat
        repResponse = either jsonToRepJson jsonToRepJson 

postBucketRefillR :: Consumer
                     -> Feature
                     -> Handler RepJson
postBucketRefillR cns feat = checkFeature cns feat $ do conn <- getConn
                                                        bm   <- liftIO . readIORef =<< getBM
                                                        let cap = unsafeGetCap bm
                                                        doRefill conn cap
                                                        jsonToRepJson $ RemainingResponse cap
  where doRefill conn cap = liftIO $ runRedis conn $ refill cns feat cap
        unsafeGetCap bm   = capacity . bucket $ bm ! (cns, feat)

postBucketDrainR :: Consumer
                    -> Feature
                    -> Handler ()
postBucketDrainR cns feat = checkFeature cns feat $ do doDrain =<< getConn 
                                                       sendNoContent
  where doDrain conn = liftIO $ runRedis conn $ drain cns feat

deleteConsumerR  :: Consumer
                    -> Handler ()
deleteConsumerR cns = checkConsumer cns $  handler >> sendNoContent
  where handler      = do bmRef <- getBM
                          conn  <- getConn
                          liftIO $ revoke bmRef conn
        revoke bmRef conn = do mapM_ backgroundKill =<< atomicModifyIORef bmRef (revokeConsumer cns)
                               runRedis conn $ deleteConsumer cns
                               backgroundDump conn =<< readIORef bmRef


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
  where switch bm     = if checkBM bm then notFoundResp else inner
        checkBM       = not . featureExists cns feat
        notFoundResp  = sendError notFound404 [("Feature Not Found", T.concat ["Could not find feature (",
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

sendNoContent :: GHandler s m a
sendNoContent = sendResponseStatus noContent204 ()

atomicAddFeature :: IORef BucketManager
                    -> Connection
                    -> Bucket
                    -> IO ()
atomicAddFeature bmRef conn bkt = do (mvar, tid) <- forkWaitingIO $ runRefiller conn bkt
                                     atomicModifyAndKill bmRef (replaceBucket bkt tid) >> unBlock mvar
  where unBlock = flip putMVar $ ()

atomicKillFeature :: IORef BucketManager
               -> Consumer
               -> Feature
               -> IO ()
atomicKillFeature bmRef cns feat = atomicModifyAndKill bmRef (revokeFeature cns feat)

atomicModifyAndKill :: IORef BucketManager
                       -> (BucketManager -> (BucketManager, Maybe ThreadId))
                       -> IO ()
atomicModifyAndKill bmRef modify = atomicModifyIORef bmRef modify >>= maybeKill
  where maybeKill (Just tid) = backgroundKill tid
        maybeKill Nothing    = return ()

backgroundKill :: ThreadId
                  -> IO ()
backgroundKill tid = forkIO (killThread tid) >> return ()

backgroundDump :: Connection
                  -> BucketManager
                  -> IO ()
backgroundDump conn bm = forkIO (runRedis conn $ storeBucketManager bm) >> return ()
