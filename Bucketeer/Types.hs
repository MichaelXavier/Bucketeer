module Bucketeer.Types (Consumer,
                        Feature,
                        Bucket(..)) where

import Data.ByteString (ByteString)

type Consumer = ByteString
type Feature  = ByteString

data Bucket = Bucket { consumer     :: Consumer,
                       feature      :: Feature,
                       capacity     :: Integer,
                       restoreRate  :: Integer } -- ^ 1 restored every restoreRate seconds
