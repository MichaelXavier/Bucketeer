{-# LANGUAGE OverloadedStrings #-}
module Bucketeer.Persistence (restore,
                              tick,
                              refill,
                              drain,
                              remaining,
                              deleteFeature,
                              deleteConsumer,
                              storeBucketManager,
                              restoreBuckets,
                              TickResult(..),
                              Response) where

import Bucketeer.Manager (BucketManager, serializeBucketManager, deserializeBucketManager)
import Bucketeer.Types

import Control.Applicative ((<$>),
                            pure)
import Control.Monad (void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack,
                              readInteger)
import Data.Maybe (fromMaybe)
import Database.Redis (hincrby,
                       del,
                       get,
                       set,
                       hset,
                       hget,
                       hdel,
                       hsetnx,
                       Redis,
                       Reply(..))

type Response a = Either ByteString a


data TickResult = TickAllowed Integer |
                  BucketExhausted deriving (Eq, Show)

restore :: BucketeerNamespace
           -> Consumer
           -> Feature
           -> Integer
           -> Redis (Response Integer)
restore ns cns feat cap = incrToCapacity =<< remaining ns cns feat
  where incrToCapacity count
          | count < cap = extractResponse <$> hincr nsk feat'
          | otherwise   = pure $ Right count
        nsk            = consumerKey ns cns
        Feature  feat' = feat

drain :: BucketeerNamespace
         -> Consumer
         -> Feature
         -> Redis ()
drain ns cns (Feature feat) = void $ hset nsk feat "0"
  where nsk = consumerKey ns cns

deleteFeature :: BucketeerNamespace
                 -> Consumer
                 -> Feature
                 -> Redis ()
deleteFeature ns cns (Feature feat) = void $ hdel nsk [feat]
  where nsk = consumerKey ns cns

deleteConsumer :: BucketeerNamespace
                  -> Consumer
                  -> Redis ()
deleteConsumer ns cns = void $ del [nsk]
  where nsk = consumerKey ns cns

refill :: BucketeerNamespace
          -> Consumer
          -> Feature
          -> Integer
          -> Redis ()
refill ns cns (Feature feat) cap = void $ hset nsk feat cap'
  where nsk  = consumerKey ns cns
        cap' = pack . show $ cap

tick :: BucketeerNamespace
        -> Consumer
        -> Feature
        -> Redis TickResult
tick ns cns feat = do _     <- hsetnx nsk feat' "0"
                      count <- remaining ns cns feat
                      if count > 0
                        then decrement >> return (TickAllowed $ count - 1)
                        else return BucketExhausted
  where decrement     = hdecr nsk feat'
        nsk           = consumerKey ns cns
        Feature feat' = feat
  
remaining :: BucketeerNamespace
             -> Consumer 
             -> Feature 
             -> Redis Integer
remaining ns cns (Feature feat) = return . cast =<< hget nsk feat 
  where cast (Left _)          = 0
        cast (Right Nothing)   = 0
        cast (Right (Just bs)) = castInt bs
        castInt                = maybe 0 fst . readInteger
        nsk                    = consumerKey ns cns

storeBucketManager :: BucketeerNamespace
                      -> BucketManager
                      -> Redis ()
storeBucketManager ns bm = void $ set mgrKey serialized
  where serialized = serializeBucketManager bm
        mgrKey     = managerKey ns

restoreBuckets :: BucketeerNamespace
                  -> Redis (Either String [Bucket])
restoreBuckets ns = loadBM <$> get mgrKey
  where loadBM (Left _)          = Left "Redis returned unexpected response"
        loadBM (Right Nothing)   = Right []
        loadBM (Right (Just bs)) = deserializeBucketManager bs
        mgrKey                   = managerKey ns


---- Helpers

hdecr :: ByteString
         -> ByteString
         -> Redis (Either Reply Integer)
hdecr key field = hincrby key field (-1)

hincr :: ByteString
         -> ByteString
         -> Redis (Either Reply Integer)
hincr key field = hincrby key field 1

consumerKey :: BucketeerNamespace
               -> Consumer
               -> ByteString
consumerKey namespace (Consumer cns) = namespacedKey namespace [bucketsNamespace, cns]

managerKey :: BucketeerNamespace
              -> ByteString
managerKey namespace = namespacedKey namespace [managerNamespace]

namespacedKey :: BucketeerNamespace
                 -> [ByteString]
                 -> ByteString
namespacedKey namespace keyParts = glue . rejectBlanks $ namespaceRoot:ns:keyParts
  where ns = fromMaybe BS.empty namespace
        rejectBlanks = filter $ not . BS.null
        glue         = BS.intercalate namespaceSep

extractResponse :: Either Reply a 
                   -> Response a
extractResponse (Right x)          = Right x
extractResponse (Left (Error str)) = Left str
extractResponse (Left x)           = Left . pack . show $ x

namespaceSep :: ByteString
namespaceSep = ":"

namespaceRoot :: ByteString
namespaceRoot = "bucketeer"

bucketsNamespace :: ByteString
bucketsNamespace = "buckets"

managerNamespace :: ByteString
managerNamespace = "manager"
