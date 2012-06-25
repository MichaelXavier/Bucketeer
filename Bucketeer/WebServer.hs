{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Bucketeer.WebServer (bucketeerServer,
                            BucketeerWeb(..)) where

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

import Control.Applicative ((<$>),
                            (<*>),
                            pure)
import Control.Concurrent (forkIO,
                           ThreadId,
                           killThread)
import Control.Concurrent.MVar (putMVar)
import Control.Monad (join,
                      void)
import Control.Monad.IO.Class (liftIO,
                               MonadIO)
import Data.Aeson (toJSON,
                   ToJSON)
import Data.ByteString (ByteString)
import Data.HashMap.Strict ((!))
import Data.IORef (readIORef,
                   IORef,
                   atomicModifyIORef)
import Data.Maybe (isJust,
                   fromJust)
import Data.Monoid (mconcat)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Database.Redis (Connection,
                       runRedis)
import Network.HTTP.Types (Status,
                           created201,
                           noContent204,
                           status400,
                           notFound404)




import Web.Scotty

bucketeerServer :: BucketeerWeb
                   -> ScottyM ()
bucketeerServer BucketeerWeb { connection    = conn,
                               bucketManager = bmRef,
                               namespace     = ns } = do
  get "/" $
    json . buckets =<< readBM

  delete "/consumers/:consumer" $ withConsumer $ \cns -> do 
    atomicRevokeConsumer bmRef ns conn cns
    sendNoContent

  get "/consumers/:consumer/buckets/:feature" $ withFeature $ \cns feat -> do 
    json =<< getRemaining conn ns cns feat

  post "/consumers/:consumer/buckets/:feature" $ withFeature $ \cns@(Consumer c)
                                                                feat@(Feature f) -> do 
    cap  <- param "capacity"
    rate <- param "restore_rate"
    create ns bmRef conn $ Bucket cns feat cap rate
    sendCreated . b2t . mconcat $ ["/consumers/", c, "/buckets/", f]

  delete "/consumers/:consumer/buckets/:feature" $ withFeature $ \cns feat -> do 
    atomicRevokeFeature bmRef ns conn cns feat
    sendNoContent

  post "/consumers/:consumer/buckets/:feature/tick" $ withFeature $ \cns feat -> do
    response <- liftIO $ runRedis conn $ tick ns cns feat
    either sendExhausted json $ tickResponse cns feat response

  post "/consumers/:consumer/buckets/:feature/refill" $ withFeature $ \cns feat -> do
    bm <- readBM
    let cap = capacity . bucket $ bm ! (cns, feat)
    liftIO $ runRedis conn $ refill ns cns feat cap

  post "/consumers/:consumer/buckets/:feature/drain" $ withFeature $ \cns feat -> do
    liftIO $ runRedis conn $ drain ns cns feat
    sendNoContent
  where readBM              = liftIO . readIORef $ bmRef
        withConsumer action = do cns <- Consumer `fmap` param "consumer"
                                 bm  <- readBM
                                 checkConsumer cns bm action
        withFeature action  = do cns  <- Consumer `fmap` param "consumer"
                                 feat <- Feature `fmap` param "feature"
                                 bm   <- readBM
                                 checkFeature cns feat bm action
        checkConsumer cns@(Consumer c)
                      bm
                      action = if consumerExists cns bm 
                                         then action cns
                                         else sendError notFound404
                                                        [("Consumer Not Found",
                                                          mconcat ["Could not find consumer ", b2t c])]
        checkFeature cns@(Consumer c)
                     feat@(Feature f)
                     bm
                     action = if consumerExists cns bm 
                                then action cns feat
                                else sendError notFound404
                                               [("Feature Not Found",
                                               mconcat ["Could not find feature (", b2t c, ", ", b2t f, ")"])]

data BucketeerWeb = BucketeerWeb { connection    :: Connection,
                                   bucketManager :: IORef BucketManager,
                                   namespace     :: BucketeerNamespace }

create :: BucketeerNamespace
          -> IORef BucketManager
          -> Connection
          -> Bucket
          -> ActionM ()
create ns bmRef conn bkt = liftIO $ do atomicAddFeature ns bmRef conn bkt
                                       runRedis conn $ refill ns cns feat $ capacity bkt
                                       backgroundDump ns conn =<< readIORef bmRef
  where cns  = consumer bkt
        feat = feature bkt

sendCreated :: Text -> ActionM ()
sendCreated path = status created201 >> header "Location" path'
  where path' = TL.fromChunks [path]

sendError :: Status
             -> [(Text, Text)]
             -> ActionM ()
sendError code errs = status code >> json responseErrors
  where responseErrors = map (uncurry ResponseError) errs

sendExhausted :: ToJSON a
                 => a
                 -> ActionM ()
sendExhausted obj = status enhanceYourCalm >> json obj

getRemaining :: Connection
                -> BucketeerNamespace
                -> Consumer
                -> Feature
                -> ActionM RemainingResponse
getRemaining conn ns cns feat = RemainingResponse `fmap` (liftIO $ runRedis conn query)
  where query = remaining ns cns feat

sendNoContent :: ActionM ()
sendNoContent = status noContent204

b2t :: ByteString
       -> Text
b2t = decodeUtf8

atomicRevokeConsumer :: IORef BucketManager
                        -> BucketeerNamespace
                        -> Connection
                        -> Consumer
                        -> ActionM ()
atomicRevokeConsumer bmRef ns conn cns = liftIO $ do mapM_ backgroundKill =<< gatherThreads
                                                     runRedis conn $ deleteConsumer ns cns
                                                     backgroundDump ns conn =<< readIORef bmRef
  where gatherThreads = atomicModifyIORef bmRef (revokeConsumer cns)

atomicRevokeFeature :: IORef BucketManager
                       -> BucketeerNamespace
                       -> Connection
                       -> Consumer
                       -> Feature
                       -> ActionM ()
atomicRevokeFeature bmRef ns conn cns feat = liftIO $ do atomicKillFeature bmRef cns feat
                                                         runRedis conn $ deleteFeature ns cns feat
                                                         backgroundDump ns conn =<< readIORef bmRef

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
