module Main (main) where

import Bucketeer.Manager (restoreBuckets,
                          storeBucketManager,
                          startBucketManager)
import Bucketeer.WebServer (BucketeerWeb(..))

import Control.Exception (finally)
import Data.IORef (newIORef,
                   readIORef)
import Database.Redis (connect,
                       runRedis,
                       defaultConnectInfo)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Exit (exitFailure)
import System.IO (hPutStrLn,
                  stderr)
import Yesod.Dispatch (toWaiApp)

main :: IO ()
main = do conn    <- connect defaultConnectInfo
          buckets <- either (exit) (return . id) =<< (runRedis conn $ restoreBuckets)
          bmRef   <- newIORef =<< startBucketManager buckets conn
          let foundation = BucketeerWeb conn bmRef
          app <- toWaiApp foundation
          (run 3000 $ logWare app) `finally` (cleanup foundation)
  where exit str = hPutStrLn stderr str >> exitFailure

---- Helpers

logWare :: Middleware
logWare = logStdoutDev

cleanup :: BucketeerWeb
           -> IO ()
cleanup BucketeerWeb { connection    = conn,
                       bucketManager = bmRef } = do bm <- readIORef bmRef
                                                    runRedis conn $ storeBucketManager bm
