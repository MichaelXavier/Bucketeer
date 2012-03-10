{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Bucketeer.WebServer (main) where

import Bucketeer.Persistence (remaining,
                              tick,
                              TickResult(..))
import Bucketeer.Types

import Data.ByteString (ByteString(..))
import Database.Redis (Connection,
                       connect,
                       runRedis,
                       hlen,
                       defaultConnectInfo)
import Network.HTTP.Types (Status(..))
import Network.Wai.Handler.Warp (run)
import Data.Text (Text(..))
import Yesod

data BucketeerWeb = BucketeerWeb { connection :: Connection }

mkYesod "BucketeerWeb" [parseRoutes|
  /consumers/#Consumer/features/#Feature BucketR GET POST
|]

instance Yesod BucketeerWeb where

getBucketR :: Consumer
              -> Feature
              -> Handler RepPlain
getBucketR cns feat = renderPlain =<< doRemaining =<< getConn
  where doRemaining conn = liftIO $ runRedis conn $ remaining cns feat 

--TODO: json responses instead
postBucketR :: Consumer
               -> Feature
               -> Handler RepPlain
postBucketR cns feat = do tickResp <- doTick =<< getConn
                          case tickResp of
                            TickAllowed n   -> renderPlain n
                            BucketExhausted -> sendResponseStatus enhanceYourCalm exhaustedResp

  where doTick conn = liftIO $ runRedis conn $ tick cns feat
        exhaustedResp = RepPlain . toContent $ ("chill" :: Text)


getConn = return . connection =<< getYesod

enhanceYourCalm :: Status
enhanceYourCalm = Status 420 "Bucket Exhausted"

renderPlain :: (Monad m, Show a) => a -> m RepPlain
renderPlain = return . RepPlain . toContent . show

main :: IO ()
main = do conn <- connect defaultConnectInfo
          run 3000 =<< toWaiApp (BucketeerWeb conn)
