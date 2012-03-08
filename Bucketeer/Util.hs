module Bucketeer.Util (forkWaitableIO,
                       applyList) where

import Control.Concurrent (forkIO,
                           ThreadId)
import Control.Concurrent.MVar (newEmptyMVar,
                                putMVar,
                                MVar)
import Control.Exception.Base (finally)
import Control.Monad.Instances

forkWaitableIO :: IO () -> IO (MVar (), ThreadId)
forkWaitableIO io = do v <- newEmptyMVar
                       tid <- forkIO $ io `finally` putMVar v ()
                       return (v, tid)

applyList :: [(a -> b)] -> a -> [b]
applyList = sequence
