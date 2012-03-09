{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bucketeer.Types (Consumer(..),
                        Feature(..),
                        Bucket(..)) where

import Data.ByteString (ByteString)
import Yesod.Dispatch (PathPiece(..))

--TODO: newtype
newtype Consumer = Consumer ByteString deriving (Show, Eq, Read)
newtype Feature  = Feature  ByteString deriving (Show, Eq, Read)

instance PathPiece Consumer
  where fromPathPiece txt = undefined
  where toPathPiece (Consumer c) = undefined

data Bucket = Bucket { consumer     :: Consumer,
                       feature      :: Feature,
                       capacity     :: Integer,
                       restoreRate  :: Integer } -- ^ 1 restored every restoreRate seconds
