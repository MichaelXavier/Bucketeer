{-# LANGUAGE OverloadedStrings     #-}
module Bucketeer.WebServer.Util (enhanceYourCalm,
                                 exhaustedResponse,
                                 tickResponse,
                                 RemainingResponse(..),
                                 ResponseError(..)) where

import Bucketeer.Types
import Bucketeer.Persistence (TickResult(..))

import Data.Aeson.Types (ToJSON(..),
                         object,
                         (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types (Status(..))

data RemainingResponse = RemainingResponse Integer

instance ToJSON RemainingResponse where
  toJSON (RemainingResponse n) = object ["remaining" .= n]

data ResponseError = ResponseError { errorId          :: Text,
                                     errorDescription :: Text } deriving (Show, Eq)

instance ToJSON ResponseError where
  toJSON ResponseError { errorId          = eid,
                         errorDescription = des } = object ["id"          .= eid,
                                                            "description" .= des]
tickResponse :: Consumer
                -> Feature
                -> TickResult
                -> Either ResponseError RemainingResponse
tickResponse _ _ (TickAllowed n)  = Right (RemainingResponse n)
tickResponse cns feat _           = Left $ exhaustedResponse cns feat

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
