{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Bucketeer.WebServer (main) where

import Bucketeer.Persistence (remaining)
import Bucketeer.Types

import Database.Redis (Connection,
                       connect,
                       runRedis,
                       hlen,
                       defaultConnectInfo)
import Network.Wai.Handler.Warp (run)
import Yesod

data BucketeerWeb = BucketeerWeb { connection :: Connection }

mkYesod "BucketeerWeb" [parseRoutes|
  /consumers/#Consumer/features/#Feature RemainingR GET
|]

instance Yesod BucketeerWeb where

getRemainingR :: Consumer -> Feature -> Handler RepPlain
getRemainingR cns feat = renderPlain =<< doRemaining =<< getConn
  where doRemaining conn = liftIO $ runRedis conn $ remaining cns feat 

getConn = return . connection =<< getYesod

renderPlain :: (Monad m, Show a) => a -> m RepPlain
renderPlain = return . RepPlain . toContent . show

main :: IO ()
main = do conn <- connect defaultConnectInfo
          run 3000 =<< toWaiApp (BucketeerWeb conn)
