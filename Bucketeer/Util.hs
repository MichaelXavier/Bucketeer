module Bucketeer.Util (forkWaitableIO,
                       forkWaitingIO,
                       toMaybe,
                       delete',
                       maybeRead,
                       applyList,
                       (.:),
                       decodeJSON) where

import Control.Concurrent (forkIO,
                           ThreadId)
import Control.Concurrent.MVar (newEmptyMVar,
                                putMVar,
                                takeMVar,
                                MVar)
import Control.Exception.Base (finally)
import Data.Aeson (json',
                   Result(..),
                   FromJSON,
                   fromJSON)
import Data.Attoparsec (eitherResult,
                        parse)
import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.Maybe (listToMaybe)

forkWaitableIO :: IO ()
                  -> IO (MVar (), ThreadId)
forkWaitableIO io = do v <- newEmptyMVar
                       tid <- forkIO $ io `finally` putMVar v ()
                       return (v, tid)

forkWaitingIO :: IO ()
                 -> IO (MVar (), ThreadId)
forkWaitingIO io = do v <- newEmptyMVar
                      tid <- forkIO $ takeMVar v >> io
                      return (v, tid)

applyList :: [a -> b]
             -> a
             -> [b]
applyList = sequence

toMaybe :: (a -> Bool)
           -> a
           -> Maybe a
toMaybe predicate x
        | predicate x = Just x
        | otherwise   = Nothing

delete' :: (Eq k, Hashable k)
           => k
           -> HashMap k v
           -> (HashMap k v, Maybe v)
delete' k h = (H.delete k h, H.lookup k h)

maybeRead :: Read a
             => String
             -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

decodeJSON :: FromJSON a
              => ByteString
              -> Either String a
decodeJSON str = fjson =<< parsed
  where parsed = eitherResult . parse json' $ str
        fjson v = case fromJSON v of
                    Success x -> Right x
                    Error e   -> Left e

(.:) :: (Functor f, Functor g)
        => (a -> b)
        -> f (g a)
        -> f (g b)
(.:) = fmap . fmap
