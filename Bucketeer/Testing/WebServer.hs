{-# LANGUAGE OverloadedStrings #-}
module Bucketeer.Testing.WebServer (specs) where

import Control.Applicative ((<$>), (<*>), pure)
import Database.Redis (connect,
                       defaultConnectInfo)

import Bucketeer.Manager (startBucketManager)
import Bucketeer.Types
import Bucketeer.WebServer (BucketeerWeb(..))

import Data.IORef (newIORef)
import Database.Redis (Connection)
import Network.Wai (Application,
                    Request(..))
import Network.Wai.Test
import Test.Hspec.Monadic (Specs,
                           describe,
                           descriptions,
                           it)
import Test.Hspec.HUnit
import Yesod (toWaiApp)

specs :: Application
         -> Specs
specs app = do
  let webApp = flip runSession $ app
  describe "request to a bogus endpoint" $ do
    it "returns a 404" $ webApp $
      assertStatus 403 =<< request defaultRequest
