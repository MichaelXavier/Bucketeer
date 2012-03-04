{-# LANGUAGE OverloadedStrings #-}
module Main where

import Bucketeer.Refiller (runRefiller)
import Bucketeer.Types
import Bucketeer.Util (forkWaitableIO)

import Control.Concurrent.MVar (takeMVar)
import Database.Redis (defaultConnectInfo)

main :: IO ()
main = do
  (mvar, _) <- forkWaitableIO $ runRefiller ci bucket
  putStrLn "Running..."
  _ <- takeMVar mvar
  return ()
  where ci = defaultConnectInfo
        bucket = Bucket { consumer = "sample_customer",
                          feature = "fun stuff",
                          capacity = 10,
                          restoreRate = 1 }
