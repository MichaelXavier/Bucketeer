{-# LANGUAGE OverloadedStrings     #-}
module Bucketeer.WebServer.Util (enhanceYourCalm,
                                 renderPlain) where

import Network.HTTP.Types (Status(..))
import Yesod.Content (RepPlain(..),
                      toContent)

enhanceYourCalm :: Status
enhanceYourCalm = Status 420 "Bucket Exhausted"

renderPlain :: (Monad m, Show a) => a -> m RepPlain
renderPlain = return . RepPlain . toContent . show
