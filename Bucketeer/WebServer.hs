{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Bucketeer.WebServer (BucketeerWeb(..)) where

import Bucketeer.Persistence (remaining,
                              tick,
                              drain,
                              refill,
                              deleteFeature,
                              storeBucketManager,
                              deleteConsumer)
import Bucketeer.Refiller (runRefiller)
import Bucketeer.Manager (BucketManager,
                          buckets,
                          featureExists,
                          consumerExists,
                          replaceBucket,
                          revokeFeature,
                          revokeConsumer,
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
import Control.Monad (join,
                      void)
import Data.Aeson (toJSON)
import Data.ByteString (ByteString)
import Data.HashMap.Strict ((!))
import Data.IORef (readIORef,
                   IORef,
                   atomicModifyIORef)
import Data.Maybe (isJust,
                   fromJust)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T
import Database.Redis (Connection,
                       runRedis)
import Network.HTTP.Types (Status,
                           noContent204,
                           status400,
                           notFound404)
import Yesod

data BucketeerWeb = BucketeerWeb { connection    :: Connection,
                                   bucketManager :: IORef BucketManager,
                                   namespace     :: BucketeerNamespace }

instance Yesod BucketeerWeb where
  approot      = ApprootRelative
  logLevel _   = LevelDebug
  encryptKey _ = return Nothing

mkYesod "BucketeerWeb" [parseRoutes|
  /                                            RootR         GET
  /consumers/#Consumer                         ConsumerR     DELETE
  /consumers/#Consumer/buckets/#Feature        BucketR       GET POST DELETE
  /consumers/#Consumer/buckets/#Feature/tick   BucketTickR   POST
  /consumers/#Consumer/buckets/#Feature/refill BucketRefillR POST
  /consumers/#Consumer/buckets/#Feature/drain  BucketDrainR  POST
|]


---- Routes

getRootR :: Handler RepJson
getRootR = jsonToRepJson . buckets =<< readBM

getBucketR :: Consumer
              -> Feature
              -> Handler RepJson
getBucketR cns feat = checkFeature cns feat $ do 
                        conn <- getConn
                        ns   <- getNS
                        jsonToRepJson . RemainingResponse =<< doRemaining conn ns
  where doRemaining conn ns = liftIO $ runRedis conn $ remainingQuery ns
        remainingQuery ns = remaining ns cns feat
 
postBucketR :: Consumer
               -> Feature
               -> Handler ()
postBucketR cns feat = do cap    <- join <$> (maybeRead . T.unpack) .: lookupPostParam "capacity"
                          rate   <- join <$> (maybeRead . T.unpack) .: lookupPostParam "restore_rate"
                          bmRef  <- getBM
                          conn   <- getConn
                          ns     <- getNS
                          if isJust cap && isJust rate
                            then create ns bmRef conn $ Bucket cns feat (fromJust cap) (fromJust rate)
                            else sendError status400 [("Missing Parameters", "capacity and restore_rate params required")]
  where create ns bmRef conn bkt = do liftIO $ do atomicAddFeature ns bmRef conn bkt
                                                  runRedis conn $ refill ns cns feat $ capacity bkt
                                                  backgroundDump ns conn =<< readIORef bmRef
                                      sendResponseCreated route
        route = BucketR cns feat

deleteBucketR :: Consumer
                 -> Feature
                 -> Handler ()
deleteBucketR cns feat = checkFeature cns feat $ handler >> sendNoContent
  where handler      = do bmRef <- getBM
                          conn  <- getConn
                          ns    <- getNS
                          liftIO $ revoke ns bmRef conn
        revoke ns bmRef conn = do atomicKillFeature bmRef cns feat
                                  runRedis conn $ deleteFeature ns cns feat
                                  backgroundDump ns conn =<< readIORef bmRef

postBucketTickR :: Consumer
                   -> Feature
                   -> Handler RepJson
postBucketTickR cns feat = checkFeature cns feat $ do
                             conn <- getConn
                             ns   <- getNS
                             checkFeature cns feat $ repResponse . tickResponse cns feat =<< doTick conn ns
  where doTick conn ns = liftIO $ runRedis conn $ tick ns cns feat
        repResponse = either exhaustedRepJson jsonToRepJson 
        exhaustedRepJson obj = sendResponseStatus enhanceYourCalm $ jsonToRep obj

postBucketRefillR :: Consumer
                     -> Feature
                     -> Handler RepJson
postBucketRefillR cns feat = checkFeature cns feat $ do conn <- getConn
                                                        ns   <- getNS
                                                        bm   <- readBM
                                                        let cap = unsafeGetCap bm
                                                        doRefill ns conn cap
                                                        jsonToRepJson $ RemainingResponse cap
  where doRefill ns conn cap = liftIO $ runRedis conn $ refill ns cns feat cap
        unsafeGetCap bm   = capacity . bucket $ bm ! (cns, feat)

postBucketDrainR :: Consumer
                    -> Feature
                    -> Handler ()
postBucketDrainR cns feat = checkFeature cns feat $ do conn <- getConn
                                                       ns   <- getNS
                                                       liftIO $ doDrain ns conn
                                                       sendNoContent
  where doDrain ns conn = runRedis conn $ drain ns cns feat

deleteConsumerR  :: Consumer
                    -> Handler ()
deleteConsumerR cns = checkConsumer cns $ handler >> sendNoContent
  where handler = do bmRef <- getBM
                     ns    <- getNS
                     conn  <- getConn
                     liftIO $ revoke bmRef ns conn
        revoke bmRef ns conn = do mapM_ backgroundKill =<< atomicModifyIORef bmRef (revokeConsumer cns)
                                  runRedis conn $ deleteConsumer ns cns
                                  backgroundDump ns conn =<< readIORef bmRef


---- Helpers
getConn :: GHandler sub BucketeerWeb Connection
getConn = connection <$> getYesod

getNS :: GHandler sub BucketeerWeb BucketeerNamespace
getNS = namespace <$> getYesod

getBM :: GHandler sub BucketeerWeb (IORef BucketManager)
getBM = bucketManager <$> getYesod

readBM :: GHandler sub BucketeerWeb BucketManager
readBM = liftIO . readIORef =<< getBM

checkConsumer :: Consumer
                 -> GHandler s BucketeerWeb b
                 -> GHandler s BucketeerWeb b
checkConsumer cns@(Consumer c) inner = switch =<< readBM
  where switch bm    = if checkBM bm then notFoundResp else inner
        checkBM      = not . consumerExists cns
        notFoundResp = sendError notFound404 [("Consumer Not Found", T.concat ["Could not find consumer ", b2t c])]

checkFeature :: Consumer
                -> Feature
                -> GHandler s BucketeerWeb b
                -> GHandler s BucketeerWeb b
checkFeature cns@(Consumer c)
             feat@(Feature f) inner = switch =<< readBM
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

atomicAddFeature :: BucketeerNamespace
                    -> IORef BucketManager
                    -> Connection
                    -> Bucket
                    -> IO ()
atomicAddFeature ns bmRef conn bkt = do (mvar, tid) <- forkWaitingIO $ runRefiller ns conn bkt
                                        atomicModifyAndKill bmRef (replaceBucket bkt tid) >> unBlock mvar
  where unBlock = flip putMVar ()

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
backgroundKill tid = void . forkIO $ killThread tid

backgroundDump :: BucketeerNamespace
                  -> Connection
                  -> BucketManager
                  -> IO ()
backgroundDump ns conn bm = void . forkIO $ runRedis conn $ storeBucketManager ns bm

jsonToRep :: ResponseError -> RepJson
jsonToRep = RepJson . toContent . toJSON
