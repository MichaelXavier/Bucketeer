{-# LANGUAGE OverloadedStrings #-}
module Bucketeer.Testing.Persistence (specs) where

import Bucketeer.Persistence
import Bucketeer.Types
import Bucketeer.Util

import Control.Exception (finally)
import Control.Monad.Instances
import Data.ByteString (ByteString(..))
import Data.ByteString.Char8 (pack)
import Database.Redis (Redis(..),
                       Connection,
                       runRedis,
                       hget,
                       del)
import Test.Hspec (Specs, describe, descriptions, it)
import Test.Hspec.HUnit
import Test.HUnit.Base (assertEqual,
                        Assertion)

specs :: Connection
         -> Specs
specs = descriptions . applyList [describe_restore]

describe_restore :: Connection -> Specs
describe_restore conn = describe "restore" [
    it "when key missing: sets the key to 1" $ withCleanup conn (doRestore >> assertRemaining conn 1)
  ]
  where doRestore = runRedis conn $ restore cns feat cap


assertRemaining :: Connection -> Integer -> IO ()
assertRemaining conn int = assertEqual message expected =<< getRemaining 
  where getRemaining = runRedis conn $ hget nsk feat
        expected     = Right (Just bsInt)
        bsInt        = pack strIint
        message      = "Remaining = " ++ strIint
        strIint      = show int

withCleanup :: Connection -> IO a -> IO a
withCleanup conn io = finally io $ runRedis conn cleanup

cap :: Integer
cap = 2

cleanup :: Redis ()
cleanup = del [nsk] >> return ()

nsk :: ByteString
nsk = "bucketeer:buckets:summer"

feat :: ByteString
feat = "barrel_roll"

cns :: ByteString
cns = "summer"
