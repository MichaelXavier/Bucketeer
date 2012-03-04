{-# LANGUAGE OverloadedStrings #-}
module Bucketeer.Persistence (restore,
                              tick, -- probably a better name
                              refill,
                              drain,
                              remaining,
                              Response) where

import Bucketeer.Types

import Control.Monad (when)
import Database.Redis -- (hincrby, Redis(..))
import Data.ByteString (ByteString(..))
import Data.ByteString as BS (concat)
import Data.ByteString.Char8 (pack, readInteger)

type Response a = Either ByteString a

restore :: Consumer
           -> Feature
           -> Integer
           -> Redis (Response Integer)
restore cns feat capacity = incrToCapacity =<< remaining cns feat
  where incrToCapacity count
          | count < capacity = return . extractResponse =<< hincr nsk feat
          | otherwise        = return $ Right count
        nsk = namespacedKey cns

drain :: Consumer
         -> Feature
         -> Redis ()
drain cns feat = hset nsk feat "0" >> return ()
  where nsk = namespacedKey cns

refill :: Consumer
         -> Feature
         -> Integer
         -> Redis ()
refill cns feat capacity = hset nsk feat capacity' >> return ()
  where nsk = namespacedKey cns
        capacity' = pack . show $ capacity

tick :: Consumer
        -> Feature
        -> Redis (Response Integer)
tick cns feat = do _ <- hsetnx nsk feat "0"
                   count <- remaining cns feat
                   when (count > 0) $ hdecr nsk feat >> return ()
                   return $ Right count
  where nsk = namespacedKey cns
  
remaining :: Consumer 
             -> Feature 
             -> Redis Integer
remaining cns feat = return . cast =<< hget nsk feat 
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
namespacedKey cns = BS.concat [namespace, namespaceSep, cns]

extractResponse :: Either Reply a 
                   -> Response a
extractResponse (Right x)          = Right x
extractResponse (Left (Error str)) = Left str
extractResponse (Left x)           = Left . pack . show $ x

namespaceSep :: ByteString
namespaceSep = ":"

namespace :: ByteString
namespace = "bucketeer"
