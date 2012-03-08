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
                       hset,
                       del)
import Test.Hspec (Specs, describe, descriptions, it)
import Test.Hspec.HUnit
import Test.HUnit.Base (assertEqual,
                        Assertion)

specs :: Connection
         -> Specs
specs = descriptions . applyList [describe_restore, describe_drain]

describe_restore :: Connection
                    -> Specs
describe_restore conn = describe "restore" [
    it "when key missing: sets the key to 1" $
       withCleanup conn (doRestore >>
                         assertRemaining conn 1),
    it "when key missing: returns the remainder" $
       withCleanup conn (doRestore >>=
                         assertResponse 1),
    it "when called twice: results in count of 2" $
       withCleanup conn (doRestore >>
                         doRestore >>
                         assertRemaining conn 2),
    it "when called twice: returns the remainder" $
       withCleanup conn (doRestore >>
                         doRestore >>=
                         assertResponse 2),
    it "when called thrice: does not exceed capacity" $
       withCleanup conn (doRestore >>
                         doRestore >>
                         doRestore >>
                         assertRemaining conn 2),
    it "when called thrice: returns the remainder bound by the capacity" $
       withCleanup conn (doRestore >>
                         doRestore >>
                         doRestore >>=
                         assertResponse 2)
  ]
  where doRestore = runRedis conn $ restore cns feat cap

describe_drain :: Connection
                  -> Specs
describe_drain conn = describe "drain" [
    it "when key is missing: sets the key to 0" $
      withCleanup conn (doDrain >>
                        assertRemaining conn 0),
    it "when key is set: sets the key to 0" $
      withCleanup conn (overwriteKey conn "15" >>
                        doDrain                >>
                        assertRemaining conn 0)
  ]
  where doDrain = runRedis conn $ drain cns feat


assertRemaining :: Connection
                   -> Integer
                   -> IO ()
assertRemaining conn int = assertEqual message expected =<< getRemaining 
  where getRemaining = runRedis conn $ hget nsk feat
        expected     = Right (Just bsInt)
        bsInt        = pack strIint
        message      = "Remaining = " ++ strIint
        strIint      = show int

assertResponse :: (Show a, Eq a)
                  => a
                  -> Response a
                  -> IO ()
assertResponse x resp = assertEqual message resp expected >> return ()
  where expected = Right x
        message  = "Response = " ++ show x

withCleanup :: Connection
               -> IO a
               -> IO a
withCleanup conn io = finally io $ runRedis conn cleanup

overwriteKey :: Connection
                -> ByteString
                -> IO ()
overwriteKey conn bs = runRedis conn $ hset nsk feat bs >> return ()

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
