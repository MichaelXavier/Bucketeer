{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Bucketeer.Manager (restoreBuckets,
                          storeBucketManager,
                          startBucketManager)
import Bucketeer.WebServer (BucketeerWeb(..))

import Control.Applicative ((<$>))
import Control.Exception (finally)
import Data.ByteString.Char8 (pack)
import Data.IORef (newIORef,
                   readIORef)
import Data.Maybe (catMaybes)
import Database.Redis (connect,
                       runRedis,
                       PortID(PortNumber),
                       ConnectInfo(..))
import Filesystem.Path.CurrentOS (encodeString)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logCallback)
import Options
import System.Exit (exitFailure)
import System.Log.FastLogger (initHandle,
                              LogStr(LB),
                              hPutLogStr)
import System.IO (hPutStrLn,
                  Handle,
                  withFile,
                  IOMode(AppendMode),
                  stdout,
                  stderr)
import Yesod.Dispatch (toWaiApp)

defineOptions "WebServerOptions" $ do
  option "port" (\o -> o { optionLongFlags   = ["port"],
                           optionShortFlags  = "p",
                           optionDescription = "Port. Default 3000",
                           optionDefault     = "3000",
                           optionType        = optionTypeInt })
  option "redisHost" (\o -> o { optionLongFlags   = ["redis-host"],
                                optionDescription = "Redis host. Default localhost",
                                optionDefault     = "localhost" })
  option "redisPort" (\o -> o { optionLongFlags   = ["redis-port"],
                                optionDescription = "Redis port. Default 6379",
                                optionDefault     = "6379",
                                optionType        = optionTypeWord16 })
  option "redisPassword" (\o -> o { optionLongFlags   = ["redis-password"],
                                    optionDescription = "Redis password. Default no password.",
                                    optionType        = optionTypeMaybe optionTypeString })
  option "redisMaxConnections" (\o -> o { optionLongFlags   = ["redis-max-connections"],
                                          optionDescription = "Redis max connections. Default 50.",
                                          optionType        = optionTypeInt,
                                          optionDefault     = "50" })
  option "redisMaxIdle" (\o -> o { optionLongFlags   = ["redis-max-idle"],
                                   optionDescription = "Redis max idle time in seconds. Default 30.",
                                   optionType        = optionTypeInt,
                                   optionDefault     = "30" })
  option "logFile" (\o -> o { optionLongFlags   = ["log-file"],
                              optionShortFlags  = "l",
                              optionDescription = "Log file. Defaults to only logging to stdout.",
                              optionType        = optionTypeMaybe optionTypeFilePath })

main :: IO ()
main = runCommand $ \opts _ -> runServer opts

---- Helpers

runServer :: WebServerOptions
             -> IO ()
runServer WebServerOptions { port                = sPort,
                             redisHost           = rHost,
                             redisPort           = rPort,
                             redisPassword       = rPass,
                             redisMaxConnections = rMaxConn,
                             redisMaxIdle        = rMaxIdle,
                             logFile             = lFile } = do
  conn    <- connect connectInfo
  buckets <- either exit (return . id) =<< runRedis conn restoreBuckets
  bmRef   <- newIORef =<< startBucketManager buckets conn
  let foundation = BucketeerWeb conn bmRef
  app     <- toWaiApp foundation
  maybeWithHandle lFile' $ \mHandle -> do
    putStrLn $ "Server listening on port " ++ show sPort
    runApp app mHandle `finally` cleanup foundation
  where connectInfo = ConnInfo { connectHost           = rHost,
                                 connectPort           = rPort',
                                 connectAuth           = rPass',
                                 connectMaxConnections = rMaxConn,
                                 connectMaxIdleTime    = rMaxIdle' }
        rPass'             = pack <$> rPass
        rPort'             = PortNumber $ fromIntegral rPort
        rMaxIdle'          = fromIntegral rMaxIdle
        lFile'             = encodeString <$> lFile
        runApp app mHandle = run sPort $ logWare mHandle app
        exit str           = hPutStrLn stderr str >> exitFailure

maybeWithHandle :: Maybe FilePath
                   -> (Maybe Handle -> IO a)
                   -> IO a
maybeWithHandle (Just fp) action = withFile fp AppendMode $ action . Just
maybeWithHandle Nothing   action = action Nothing

logWare :: Maybe Handle
           -> Middleware
logWare fileHandle = logCallback writeLogs
  where handles         = catMaybes [fileHandle, Just stdout]
        writeLogs bs    = mapM_ initHandle handles >> mapM_ (putLog bs) handles
        putLog bs h     = hPutLogStr h [LB bs]

cleanup :: BucketeerWeb
           -> IO ()
cleanup BucketeerWeb { connection    = conn,
                       bucketManager = bmRef } = do bm <- readIORef bmRef
                                                    runRedis conn $ storeBucketManager bm
