{-# LANGUAGE LambdaCase #-}

module S2RA.Concurrent
  ( JoinHandle(..)
  , forkJoinable
  , joinHandle
  , joinHandle_
  , joinTid
  , module Control.Concurrent
  ) where

import Control.Monad ( void )
import Control.Concurrent
import GHC.Exception

newtype JoinHandle a = JH (ThreadId, MVar a)

-- WARNING: In GHC, holding onto the ThreadID after a thread has finished
--          running causes a memory leak.
forkJoinable :: IO a -> IO (JoinHandle a)
forkJoinable action = do
  lock <- newEmptyMVar :: IO (MVar a)
  tid  <- forkFinally action (\ case
    Right x -> putMVar lock x
    Left e  -> throw e)

  return $ JH (tid, lock)

joinHandle :: JoinHandle a -> IO a
joinHandle (JH (_, lock)) = takeMVar lock

joinHandle_ :: JoinHandle a -> IO ()
joinHandle_ = void . joinHandle

joinTid :: JoinHandle a -> ThreadId
joinTid (JH (tid, _)) = tid
