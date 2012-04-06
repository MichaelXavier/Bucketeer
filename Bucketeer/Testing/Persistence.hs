{-# LANGUAGE OverloadedStrings #-}
module Bucketeer.Testing.Persistence (specs) where

import Bucketeer.Persistence
import Bucketeer.Types
import Bucketeer.Util

import Control.Exception (bracket_)
import Control.Monad (void)
import Control.Monad.Instances
import Data.ByteString (ByteString(..))
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack,
                              unpack)
import Database.Redis (Redis(..),
                       Connection,
                       runRedis,
                       exists,
                       hget,
                       hexists,
                       hset,
                       del)
import Test.Hspec (Specs,
                   describe,
                   descriptions,
                   it)
import Test.Hspec.HUnit
import Test.HUnit.Base (assertEqual,
                        Assertion)

specs :: Connection
         -> Specs
specs = descriptions . applyList [describe_restore,
                                  describe_drain,
                                  describe_refill,
                                  describe_remaining,
                                  describe_tick,
                                  describe_deleteFeature,
                                  describe_deleteConsumer]

describe_restore :: Connection
                    -> Specs
describe_restore conn = describe "Bucketeer.Persistence.restore" [
    it "when key missing: sets the key to 1" $
       withCleanup conn $ do doRestore
                             assertRemaining conn 1,
    it "when key missing: returns the remainder" $
       withCleanup conn (assertResponse 1 =<< doRestore), 
    it "when called twice: results in count of 2" $
       withCleanup conn $ do doRestore
                             doRestore
                             assertRemaining conn 2,
    it "when called twice: returns the remainder" $
       withCleanup conn $ do doRestore
                             doRestore >>= assertResponse 2,
    it "when called thrice: does not exceed capacity" $
       withCleanup conn $ do doRestore
                             doRestore
                             doRestore
                             assertRemaining conn 2,
    it "when called thrice: returns the remainder bound by the capacity" $
       withCleanup conn $ do doRestore
                             doRestore
                             doRestore >>= assertResponse 2
  ]
  where doRestore = runRedis conn $ restore ns cns feat cap

describe_drain :: Connection
                  -> Specs
describe_drain conn = describe "Bucketeer.Persistence.drain" [
    it "when key is missing: sets the key to 0" $
      withCleanup conn $ do doDrain
                            assertRemaining conn 0,
    it "when key is set: sets the key to 0" $
      withCleanup conn $ do overwriteKey conn "15"
                            doDrain
                            assertRemaining conn 0
  ]
  where doDrain = runRedis conn $ drain ns cns feat

describe_refill :: Connection
                  -> Specs
describe_refill conn = describe "Bucketeer.Persistence.refill" [
    it "when key is missing: sets the bucket to capacity" $
      withCleanup conn $ do doRefill
                            assertRemaining conn 2,
    it "when key is set: sets the bucket to capacity" $
      withCleanup conn $ do overwriteKey conn "15"
                            doRefill
                            assertRemaining conn 2
  ]
  where doRefill = runRedis conn $ refill ns cns feat cap

describe_remaining :: Connection
                  -> Specs
describe_remaining conn = describe "Bucketeer.Persistence.remaining" [
    it "when key is missing: returns 0" $
      withCleanup conn $ assertEqual "equals 0 " 0 =<< getRemaining,
    it "when key is set: returns the key" $
      withCleanup conn $ do overwriteKey conn "15"
                            getRemaining >>= assertEqual "equals 15 " 15,
    it "when key is corrupted: returns 0" $
      withCleanup conn $ do overwriteKey conn "bogus"
                            getRemaining >>= assertEqual "equals 0" 0
  ]
  where getRemaining = runRedis conn $ remaining ns cns feat

describe_tick :: Connection
                  -> Specs
describe_tick conn = describe "Bucketeer.Persistence.tick" [
    it "when key is missing: sets the key to 0" $
      withCleanup conn $ do doTick
                            assertRemaining conn 0,
    it "when key is missing: returns BucketExhausted" $ 
      withCleanup conn $ assertEqual "BucketExhausted" BucketExhausted =<< doTick,
    it "when key is 0: leaves the key at 0" $
      withCleanup conn $ do overwriteKey conn "0"
                            doTick
                            assertRemaining conn 0,
    it "when key is 0: returns BucketExhausted" $ 
      withCleanup conn $ assertEqual "BucketExhausted" BucketExhausted =<< doTick,
    it "when key is > 0: decrements the key" $
      withCleanup conn $ do overwriteKey conn "2"
                            doTick
                            assertRemaining conn 1,
    it "when key is > 0: returns TickAllowed with the remaining count" $ 
      withCleanup conn $ do overwriteKey conn "2"
                            doTick >>= assertEqual "TickAllowed 1" (TickAllowed 1)
  ]
  where doTick = runRedis conn $ tick ns cns feat


describe_deleteFeature :: Connection
                          -> Specs
describe_deleteFeature conn = describe "Bucketeer.Persistence.deleteFeature" [
    it "when key is missing: keeps the key missing?" $
      withCleanup conn $ do overwriteKey conn "2"
                            doDeleteFeature conn cns $ Feature "wat"
                            assertDeletedBucket conn $ Feature "wat",
    it "when key is present: deletes the key" $
      withCleanup conn $ do overwriteKey conn "2"
                            doDeleteFeature conn cns feat
                            assertDeletedBucket conn feat
  ]
  where assertDeletedBucket conn (Feature f) = assertEqual (assertMsg f) (Right False) =<<
                                               runRedis conn (hexists "bucketeer:test:summer" f)
        assertMsg f                          = "Deleted " ++ unpack f
        doDeleteFeature conn cns feat = runRedis conn $ deleteFeature ns cns feat

describe_deleteConsumer :: Connection
                           -> Specs
describe_deleteConsumer conn = describe "Bucketeer.Persistence.deleteConsumer" [
    it "when key is missing: keeps the key missing?" $
      withCleanup conn $ do doDeleteConsumer conn (Consumer "bogus")
                            assertDeletedConsumer conn (Consumer "bogus"),
    it "when key is present: deletes the key" $
      withCleanup conn $ do overwriteKey conn "2"
                            doDeleteConsumer conn cns
                            assertDeletedConsumer conn cns
  ]
  where assertDeletedConsumer conn (Consumer c) = assertEqual (assertMsg c) (Right False) =<<
                                                  runRedis conn (exists $ BS.concat ["bucketeer:test:buckets:", c])
        assertMsg c                             = "Deleted " ++ unpack c
        doDeleteConsumer conn cns = runRedis conn $ deleteConsumer ns cns

--- Helpers

assertRemaining :: Connection
                   -> Integer
                   -> IO ()
assertRemaining conn int = assertEqual message expected =<< getRemaining 
  where getRemaining = runRedis conn $ hget nsk feat'
        expected     = Right (Just bsInt)
        bsInt        = pack strIint
        message      = "Remaining = " ++ strIint
        strIint      = show int

assertResponse :: (Show a, Eq a)
                  => a
                  -> Response a
                  -> IO ()
assertResponse x resp = void $ assertEqual message resp expected
  where expected = Right x
        message  = "Response = " ++ show x

withCleanup :: Connection
               -> IO a
               -> IO a
withCleanup conn io = bracket_ clean clean io
  where clean = runRedis conn cleanup

overwriteKey :: Connection
                -> ByteString
                -> IO ()
overwriteKey conn bs = runRedis conn $ void $ hset nsk feat' bs

cap :: Integer
cap = 2

cleanup :: Redis ()
cleanup = void $ del [nsk]

nsk :: ByteString
nsk = "bucketeer:test:buckets:summer"

feat :: Feature
feat = Feature feat'

feat' :: ByteString
feat' = "barrel_roll"

cns :: Consumer
cns = Consumer cns'

cns' :: ByteString
cns' = "summer"

ns :: Maybe ByteString
ns = Just "test"
