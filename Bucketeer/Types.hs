{-# LANGUAGE TypeSynonymInstances           #-}
module Bucketeer.Types (Consumer(..),
                        Feature(..),
                        Bucket(..)) where

import Data.ByteString (ByteString)
import Yesod.Dispatch (PathPiece)

--TODO: newtype
newtype Consumer = Consumer ByteString
newtype Feature  = Feature  ByteString

data Bucket = Bucket { consumer     :: Consumer,
                       feature      :: Feature,
                       capacity     :: Integer,
                       restoreRate  :: Integer } -- ^ 1 restored every restoreRate seconds
