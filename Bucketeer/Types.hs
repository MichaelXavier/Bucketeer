module Bucketeer.Types (Consumer(..),
                        Feature(..),
                        Bucket(..)) where

import Bucketeer.Util (toMaybe)

import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (null)
import Data.Text.Encoding (decodeUtf8,
                           encodeUtf8)
import Yesod.Dispatch (PathPiece(..))

--TODO: newtype
newtype Consumer = Consumer ByteString deriving (Show, Eq, Read)
newtype Feature  = Feature  ByteString deriving (Show, Eq, Read)

instance PathPiece Consumer where
  fromPathPiece txt = Consumer <$> toMaybe (not . BS.null) bs
    where bs = encodeUtf8 txt
  toPathPiece (Consumer cns) = decodeUtf8 cns

instance PathPiece Feature where
  fromPathPiece txt = Feature <$> toMaybe (not . BS.null) bs
    where bs = encodeUtf8 txt
  toPathPiece (Feature feat) = decodeUtf8 feat

data Bucket = Bucket { consumer     :: Consumer,
                       feature      :: Feature,
                       capacity     :: Integer,
                       restoreRate  :: Integer } -- ^ 1 restored every restoreRate seconds
