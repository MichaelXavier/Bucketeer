{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Bucketeer.WebServer (main) where

import Database.Redis (Connection,
                       connect,
                       runRedis,
                       hlen,
                       defaultConnectInfo)
import Network.Wai.Handler.Warp (run)
import Yesod

data BucketeerWeb = BucketeerWeb { connection :: Connection }

mkYesod "BucketeerWeb" [parseRoutes|
  / RootR GET
|]

instance Yesod BucketeerWeb where

getRootR :: Handler RepPlain
getRootR = do BucketeerWeb { connection = conn } <- getYesod
              Right len <- liftIO $ runRedis conn $ hlen "bucketeer:buckets:farge"
              return $ RepPlain $ toContent $ show len

main :: IO ()
main = do conn <- connect defaultConnectInfo
          run 3000 =<< toWaiApp (BucketeerWeb conn)
