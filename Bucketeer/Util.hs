module Bucketeer.Util (forkWaitableIO,
                       toMaybe,
                       delete',
                       applyList,
                       decodeJSON) where

import Control.Applicative ((<*))
import Control.Concurrent (forkIO,
                           ThreadId)
import Control.Concurrent.MVar (newEmptyMVar,
                                putMVar,
                                MVar)
import Data.Aeson (json',
                   Result(..),
                   FromJSON,
                   fromJSON)
import Data.Attoparsec (eitherResult,
                        parse)
import Data.Aeson.Types (ToJSON,
                         toJSON)
import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Control.Exception.Base (finally)
import Control.Monad.Instances

forkWaitableIO :: IO ()
                  -> IO (MVar (), ThreadId)
forkWaitableIO io = do v <- newEmptyMVar
                       tid <- forkIO $ io `finally` putMVar v ()
                       return (v, tid)

applyList :: [(a -> b)]
             -> a
             -> [b]
applyList = sequence

toMaybe :: (a -> Bool)
           -> a
           -> Maybe a
toMaybe pred x
        | pred x    = Just x
        | otherwise = Nothing

delete' :: (Eq k, Hashable k)
           => k
           -> HashMap k v
           -> (HashMap k v, Maybe v)
delete' k h = (H.delete k h, H.lookup k h)

decodeJSON :: FromJSON a
              => ByteString
              -> Either String a
decodeJSON str = fjson =<< parsed
  where parsed = eitherResult . parse json' $ str
        fjson v = case fromJSON v of
                    Success x -> Right x
                    Error e   -> Left e
