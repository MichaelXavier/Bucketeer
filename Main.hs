{-# LANGUAGE OverloadedStrings #-}
module Main where

import Bucketeer.Types
import Bucketeer.Manager
import Bucketeer.Persistence

import Control.Concurrent
import Database.Redis

main :: IO ()
main = do conn <- connect ci
          tid <- myThreadId
          let bm = addBucket bucket tid defaultBucketManager
          runRedis conn $ storeBucketManager bm
          return ()
  where ci = defaultConnectInfo
        bucket = Bucket { consumer    = Consumer "sample_customer",
                          feature     = Feature "fun stuff",
                          capacity    = 10,
                          restoreRate = 1 }
