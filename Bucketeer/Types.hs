{-# LANGUAGE OverloadedStrings #-}
module Bucketeer.Types (Consumer(..),
                        Feature(..),
                        Bucket(..)) where

import Bucketeer.Util (toMaybe)

import Control.Applicative ((<$>),
                            pure,
                            (<*>))
import Data.Aeson.Encode (encode) --TODO: move to Manager
import Data.Aeson.Types (FromJSON,
                         parseJSON,
                         ToJSON,
                         toJSON,
                         object,
                         Value(..),
                         typeMismatch,
                         (.=),
                         (.:))

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (null)
import Data.Hashable (Hashable(..))
import Data.Text.Encoding (decodeUtf8,
                           encodeUtf8)
import Yesod.Dispatch (PathPiece(..))


newtype Consumer = Consumer ByteString deriving (Show, Eq, Read)

instance ToJSON Consumer where
  toJSON (Consumer cns) = toJSON cns

instance FromJSON Consumer where
  parseJSON (String str) = pure $ Consumer . encodeUtf8 $ str
  parseJSON v            = typeMismatch "String" v

instance PathPiece Consumer where
  fromPathPiece txt = Consumer <$> toMaybe (not . BS.null) bs
    where bs = encodeUtf8 txt
  toPathPiece (Consumer cns) = decodeUtf8 cns

instance Hashable Consumer where
  hash (Consumer cns)           = hash cns
  hashWithSalt n (Consumer cns) = hashWithSalt n cns

newtype Feature  = Feature  ByteString deriving (Show, Eq, Read)

instance ToJSON Feature where
  toJSON (Feature feat) = toJSON feat

instance FromJSON Feature where
  parseJSON v = Feature <$> parseJSON v

instance Hashable Feature where
  hash (Feature feat)           = hash feat
  hashWithSalt n (Feature feat) = hashWithSalt n feat

instance PathPiece Feature where
  fromPathPiece txt = Feature <$> toMaybe (not . BS.null) bs
    where bs = encodeUtf8 txt
  toPathPiece (Feature feat) = decodeUtf8 feat

data Bucket = Bucket { consumer     :: Consumer,
                       feature      :: Feature,
                       capacity     :: Integer,
                       restoreRate  :: Integer } deriving (Show, Eq) -- ^ 1 restored every restoreRate second

instance ToJSON Bucket where
  toJSON Bucket { consumer     = cns,
                  feature      = feat,
                  capacity     = cap,
                  restoreRate  = rate } = object ["consumer"     .= cns,
                                                  "feature"      .= feat,
                                                  "capacity"     .= cap,
                                                  "restore_rate" .= rate ]

instance FromJSON Bucket where
  parseJSON (Object v) = Bucket <$> v .: "consumer"
                                <*> v .: "feature"
                                <*> v .: "capacity"
                                <*> v .: "restore_rate"
  parseJSON v          = typeMismatch "Object" v
