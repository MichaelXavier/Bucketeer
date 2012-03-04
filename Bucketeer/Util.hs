module Bucketeer.Util (forkWaitableIO) where

import Control.Concurrent (forkIO,
                           ThreadId)
import Control.Concurrent.MVar (newEmptyMVar,
                                putMVar,
                                MVar)
import Control.Exception.Base (finally)

forkWaitableIO :: IO () -> IO (MVar (), ThreadId)
forkWaitableIO io = do v <- newEmptyMVar
                       tid <- forkIO $ io `finally` putMVar v ()
                       return (v, tid)
