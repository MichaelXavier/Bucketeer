{-# LANGUAGE OverloadedStrings     #-}
module Bucketeer.WebServer.Util (enhanceYourCalm,
                                 exhaustedResponse,
                                 ResponseError(..),
                                 renderPlain) where

import Bucketeer.Types

import Data.Aeson.Types (ToJSON(..),
                         object,
                         (.=))
import Data.Text (Text(..))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types (Status(..))
import Yesod.Content (RepPlain(..),
                      toContent)

data ResponseError = ResponseError { errorId          :: Text,
                                     errorDescription :: Text} deriving (Show, Eq)

instance ToJSON ResponseError where
  toJSON ResponseError { errorId          = eid,
                         errorDescription = des}  = object ["id"          .= eid,
                                                            "description" .= des]

enhanceYourCalm :: Status
enhanceYourCalm = Status 420 "Bucket Exhausted"

exhaustedResponse :: Consumer
                     -> Feature
                     -> ResponseError
exhaustedResponse (Consumer cns) (Feature feat) = ResponseError { errorId          = "Bucket Exhausted",
                                                                  errorDescription = desc }
  where desc  = T.concat [feat', " bucket has been exhausted for ", cns']
        feat' = decodeUtf8 feat
        cns'  = decodeUtf8 cns

renderPlain :: (Monad m, Show a)
               => a
               -> m RepPlain
renderPlain = return . RepPlain . toContent . show
