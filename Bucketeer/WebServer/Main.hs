{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Bucketeer.Persistence (storeBucketManager,
                              restoreBuckets)
import Bucketeer.Timers (startBucketManager)
import Bucketeer.WebServer (BucketeerWeb(..), bucketeerServer)

import Control.Applicative ((<$>),
                            (<*>),
                            pure)
import Control.Exception (finally)
import Data.ByteString.Char8 (pack)
import Data.IORef (newIORef,
                   readIORef)
import Data.Maybe (catMaybes,
                   fromMaybe)
import Database.Redis (connect,
                       runRedis,
                       PortID(PortNumber),
                       ConnectInfo(..))
import Filesystem.Path.CurrentOS (encodeString)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logCallback)
import Options
import System.Posix.Env (getEnvDefault)
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

import Web.Scotty

defineOptions "WebServerOptions" $ do
  option "port" (\o -> o { optionLongFlags   = ["port"],
                           optionShortFlags  = "p",
                           optionDescription = "Port. Default 3000, can also be set with PORT env var (for keter deployment)",
                           optionType        = optionTypeMaybe optionTypeInt })
  option "namespaceOpt" (\o -> o { optionLongFlags   = ["namespcae"],
                                   optionShortFlags  = "n",
                                   optionDescription = "Optional redis namespace so multiple bucketeers can run on the same Redis instance",
                                   optionType        = optionTypeMaybe optionTypeString })
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
                             namespaceOpt        = ns,
                             redisHost           = rHost,
                             redisPort           = rPort,
                             redisPassword       = rPass,
                             redisMaxConnections = rMaxConn,
                             redisMaxIdle        = rMaxIdle,
                             logFile             = lFile } = do
  conn    <- connect connectInfo
  buckets <- either exit (return . id) =<< runRedis conn (restoreBuckets ns')
  bmRef   <- newIORef =<< startBucketManager buckets ns' conn
  let foundation = BucketeerWeb conn bmRef ns'
  app     <- scottyApp $ bucketeerServer foundation
  sPort'  <- fromMaybe <$> getPortFromEnv <*> pure sPort
  maybeWithHandle lFile' $ \mHandle -> do
    putStrLn $ "Server listening on port " ++ show sPort'
    runApp app mHandle sPort' `finally` cleanup foundation
  where connectInfo = ConnInfo { connectHost           = rHost,
                                 connectPort           = rPort',
                                 connectAuth           = rPass',
                                 connectMaxConnections = rMaxConn,
                                 connectMaxIdleTime    = rMaxIdle' }
        ns'                       = pack <$> ns
        rPass'                    = pack <$> rPass
        rPort'                    = PortNumber $ fromIntegral rPort
        rMaxIdle'                 = fromIntegral rMaxIdle
        lFile'                    = encodeString <$> lFile
        runApp app mHandle sPort' = run sPort' $ logWare mHandle app
        exit str                  = hPutStrLn stderr str >> exitFailure
        getPortFromEnv            = read `fmap` getEnvDefault "PORT" "3000"

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
                       bucketManager = bmRef,
                       namespace     = ns } = do bm <- readIORef bmRef
                                                 runRedis conn $ storeBucketManager ns bm
