{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Bucketeer.Persistence (restore,
                              tick,
                              refill,
                              drain,
                              remaining,
                              deleteFeature,
                              deleteConsumer,
                              TickResult(..),
                              Response) where

import Bucketeer.Types

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack,
                              readInteger)
import Database.Redis (hincrby,
                       del,
                       hset,
                       hget,
                       hdel,
                       hsetnx,
                       Redis,
                       Reply(..))

type Response a = Either ByteString a

data TickResult = TickAllowed Integer |
                  BucketExhausted deriving (Eq, Show)

restore :: Consumer
           -> Feature
           -> Integer
           -> Redis (Response Integer)
restore cns feat cap = incrToCapacity =<< remaining cns feat
  where incrToCapacity count
          | count < cap = return . extractResponse =<< hincr nsk feat'
          | otherwise   = return $ Right count
        nsk            = namespacedKey cns
        Feature  feat' = feat

drain :: Consumer
         -> Feature
         -> Redis ()
drain cns (Feature feat) = hset nsk feat "0" >> return ()
  where nsk = namespacedKey cns

deleteFeature :: Consumer
                 -> Feature
                 -> Redis ()
deleteFeature cns (Feature feat) = hdel nsk [feat] >> return ()
  where nsk = namespacedKey cns

deleteConsumer :: Consumer
                  -> Redis ()
deleteConsumer cns = del [nsk] >> return ()
  where nsk = namespacedKey cns

refill :: Consumer
          -> Feature
          -> Integer
          -> Redis ()
refill cns (Feature feat) cap = hset nsk feat cap' >> return ()
  where nsk       = namespacedKey cns
        cap' = pack . show $ cap

tick :: Consumer
        -> Feature
        -> Redis TickResult
tick cns feat = do _     <- hsetnx nsk feat' "0"
                   count <- remaining cns feat
                   if count > 0 then
                     decrement >> return (TickAllowed $ count - 1)
                   else
                     return BucketExhausted
  where decrement     = hdecr nsk feat'
        nsk           = namespacedKey cns
        Feature feat' = feat
  
remaining :: Consumer 
             -> Feature 
             -> Redis Integer
remaining cns (Feature feat) = return . cast =<< hget nsk feat 
  where cast (Left _)          = 0
        cast (Right Nothing)   = 0
        cast (Right (Just bs)) = castInt bs
        castInt                = (maybe 0 fst) . readInteger
        nsk                    = namespacedKey cns

---- Helpers

hdecr :: ByteString
         -> ByteString
         -> Redis (Either Reply Integer)
hdecr key field = hincrby key field (-1)

hincr :: ByteString
         -> ByteString
         -> Redis (Either Reply Integer)
hincr key field = hincrby key field 1

namespacedKey :: Consumer
                 -> ByteString
namespacedKey (Consumer cns) = BS.concat [namespace, namespaceSep, cns]

extractResponse :: Either Reply a 
                   -> Response a
extractResponse (Right x)          = Right x
extractResponse (Left (Error str)) = Left str
extractResponse (Left x)           = Left . pack . show $ x

namespaceSep :: ByteString
namespaceSep = ":"

namespace :: ByteString
namespace = "bucketeer:buckets"
