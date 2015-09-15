{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK not-home #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2015 Edward Kmett and Ted Cooper
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>, 
--                Ted Cooper <anthezium@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- QSBR-based RCU
-----------------------------------------------------------------------------
module Control.Concurrent.RCU.QSBR.Internal
  ( SRef(..)
  , RCUThread(..)
  , RCU(..)
  , runRCU
  , ReadingRCU(..)
  , WritingRCU(..)
  , RCUState(..)
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.RCU.Class
import Control.Monad
import Control.Monad.IO.Class
import Control.Parallel
import Data.Atomics 
import Data.List
import Data.IORef 
import Data.Word
import Prelude hiding (read, Read)

--------------------------------------------------------------------------------
-- * Shared References
--------------------------------------------------------------------------------

-- | Shared references
newtype SRef s a = SRef { unSRef :: IORef a }
  deriving Eq

newSRefIO :: a -> IO (IORef a)
newSRefIO = newIORef
{-# INLINE newSRefIO #-}

readSRefIO :: IORef a -> IO a
readSRefIO = readIORef
{-# INLINE readSRefIO #-}

writeSRefIO :: IORef a -> a ->  IO ()
writeSRefIO r a = do a `pseq` writeBarrier
                     writeIORef r a
{-# INLINE writeSRefIO #-}

--------------------------------------------------------------------------------
-- * Shared state
--------------------------------------------------------------------------------

-- | Counter for causal ordering.
type Counter = IORef Word64

offline :: Word64
offline = 0

online :: Word64
online  = 1

-- counterInc :: Word64
-- counterInc = 2 -- online threads will never overflow to 1

newCounter :: IO Counter
newCounter = newIORef online

readCounter :: Counter -> IO Word64
readCounter = readIORef
{-# INLINE readCounter #-}

writeCounter :: Counter -> Word64 -> IO ()
writeCounter c !i = writeIORef c i
{-# INLINE writeCounter #-}

incCounter :: Counter -> IO Word64
incCounter c = do !x <- succ <$> readIORef c
                  writeCounter c x
                  return x
{-# INLINE incCounter #-}
  

-- | State for an RCU computation.
data RCUState = RCUState
  { rcuStateGlobalCounter   :: {-# UNPACK #-} !Counter
  , rcuStateMyCounter       :: {-# UNPACK #-} !Counter  -- each thread's state gets its own counter
  , rcuStateThreadCountersV :: {-# UNPACK #-} !(MVar [Counter])
  , rcuStateWriterLockV     :: {-# UNPACK #-} !(MVar ())
  }

--------------------------------------------------------------------------------
-- * Read-Side Critical Sections
--------------------------------------------------------------------------------

-- | This is the basic read-side critical section for an RCU computation
newtype ReadingRCU s a = ReadingRCU { runReadingRCU :: RCUState -> IO a } 
  deriving Functor

instance Applicative (ReadingRCU s) where
  pure a = ReadingRCU $ \ _ -> pure a
  ReadingRCU mf <*> ReadingRCU ma = ReadingRCU $ \ s -> mf s <*> ma s

instance Monad (ReadingRCU s) where
  return a = ReadingRCU $ \ _ -> pure a
  ReadingRCU m >>= f = ReadingRCU $ \ s -> do
    a <- m s
    runReadingRCU (f a) s
  fail s = ReadingRCU $ \ _ -> fail s

instance Alternative (ReadingRCU s) where
  empty = ReadingRCU $ \ _ -> empty
  ReadingRCU ma <|> ReadingRCU mb = ReadingRCU $ \s -> ma s <|> mb s

instance MonadPlus (ReadingRCU s) where
  mzero = ReadingRCU $ \ _ -> mzero
  ReadingRCU ma `mplus` ReadingRCU mb = ReadingRCU $ \s -> ma s `mplus` mb s

instance MonadNew (SRef s) (ReadingRCU s) where
  newSRef a = ReadingRCU $ \_ -> SRef <$> newSRefIO a

instance MonadReading (SRef s) (ReadingRCU s) where
  readSRef (SRef r) = ReadingRCU $ \ _ -> readSRefIO r
  {-# INLINE readSRef #-}

--------------------------------------------------------------------------------
-- * Write-Side Critical Sections
--------------------------------------------------------------------------------

-- | This is the basic write-side critical section for an RCU computation
newtype WritingRCU s a = WritingRCU { runWritingRCU :: RCUState -> IO a }
  deriving Functor

instance Applicative (WritingRCU s) where
  pure a = WritingRCU $ \ _ -> pure a
  WritingRCU mf <*> WritingRCU ma = WritingRCU $ \ s -> mf s <*> ma s

instance Monad (WritingRCU s) where
  return a = WritingRCU $ \ _ -> pure a
  WritingRCU m >>= f = WritingRCU $ \ s -> do
    a <- m s
    runWritingRCU (f a) s
  fail s = WritingRCU $ \ _ -> fail s

instance Alternative (WritingRCU s) where
  empty = WritingRCU $ \ _ -> empty
  WritingRCU ma <|> WritingRCU mb = WritingRCU $ \s -> ma s <|> mb s

instance MonadPlus (WritingRCU s) where
  mzero = WritingRCU $ \ _ -> mzero
  WritingRCU ma `mplus` WritingRCU mb = WritingRCU $ \s -> ma s `mplus` mb s

instance MonadNew (SRef s) (WritingRCU s) where
  newSRef a = WritingRCU $ \_ -> SRef <$> newSRefIO a

instance MonadReading (SRef s) (WritingRCU s) where
  readSRef (SRef r) = WritingRCU $ \ _ -> readSRefIO r
  {-# INLINE readSRef #-}

instance MonadWriting (SRef s) (WritingRCU s) where
  writeSRef (SRef r) a = WritingRCU $ \ _ -> writeSRefIO r a
  {-# INLINE writeSRef #-}
  synchronize = WritingRCU synchronizeIO

synchronizeIO :: RCUState -> IO () 
synchronizeIO RCUState { rcuStateGlobalCounter
                       , rcuStateMyCounter
                       , rcuStateThreadCountersV } = do
  -- Get this thread's counter.
  mc <- readCounter rcuStateMyCounter
  storeLoadBarrier
  -- If this thread is not offline already, take it offline.
  when (mc /= offline) $ writeCounter rcuStateMyCounter offline

  -- Loop through thread counters, waiting for online threads to catch up
  -- and skipping offline threads.  
  -- TODO: urcu acquires rcu_gp_lock here, and holds it until the writer has 
  -- updated the global counter AND finished waiting for readers. I think we may be able
  -- to avoid holding the lock while waiting for readers as long as we
  -- allow thread counters to exceed gc' (this is the case currently).     
  -- Maybe urcu holds the lock to prevent multiple readers from busy-waiting
  -- on the same shared array?  All they're doing is loading, so I'm not sure
  -- why that would be a bad thing...  This issue, (and having a lock here at all)
  -- is moot until we remove the big lock around write-side critical sections.
  -- This is worth pinging Mathieu about.
  -- So, TODO: Remove this lock, add a storeLoadBarrier after rcuStateMyCounter update.
  gc' <- withMVar rcuStateThreadCountersV $ \ threadCounters -> do
    -- Increment the global counter.
    gc' <- incCounter rcuStateGlobalCounter
    writeBarrier
    -- Wait for each online reader to copy the new global counter.
    let waitForThread i threadCounter = do
          tc <- readCounter threadCounter
          when (tc /= offline && tc < gc') $ do 
            -- urcu puts the thread on a futex wait queue once per Int32 overflow
            -- in this loop.  
            threadDelay 1    -- TODO: Busy-wait for a while before sleeping.

            storeLoadBarrier -- This works on all systems, even those with 
                             -- incoherent caches, but slows down writers 
                             -- unnecessarily on cache-coherent systems.  
                             -- TODO: On cache-coherent systems, 
                             -- figure out how to make GHC emit e.g. "rep; nop"
                             -- to tell the CPU we're in a busy-wait loop.  
                             -- urcu uses "caa_cpu_relax()" decorated with a compiler
                             -- reordering barrier in this case.
            waitForThread (i + 1) threadCounter
    forM_ threadCounters (waitForThread (0 :: Int))
    return gc'
  when (mc /= offline) $ writeCounter rcuStateMyCounter gc'
  storeLoadBarrier

--------------------------------------------------------------------------------
-- * RCU Context
--------------------------------------------------------------------------------

-- | This is an RCU computation. It can use 'forking' and 'joining' to form
-- new threads, and then you can use 'reading' and 'writing' to run classic
-- read-side and write-side RCU computations. Writers are
-- serialized using an MVar, readers are able to proceed while writers are
-- updating.
newtype RCU s a = RCU { unRCU :: RCUState -> IO a }
  deriving Functor

instance Applicative (RCU s) where
  pure = return
  (<*>) = ap

instance Monad (RCU s) where
  return a = RCU $ \ _ -> return a
  RCU m >>= f = RCU $ \s -> do
    a <- m s
    unRCU (f a) s

instance MonadNew (SRef s) (RCU s) where
  newSRef a = RCU $ \_ -> SRef <$> newSRefIO a

-- | This is a basic 'RCU' thread. It may be embellished when running in a more
-- exotic context.
data RCUThread s a = RCUThread
  { rcuThreadId  :: {-# UNPACK #-} !ThreadId
  , rcuThreadVar :: {-# UNPACK #-} !(MVar a)
  }

instance MonadRCU (SRef s) (RCU s) where
  type Reading (RCU s) = ReadingRCU s
  type Writing (RCU s) = WritingRCU s
  type Thread (RCU s) = RCUThread s
  forking (RCU m) = RCU $ \ s@RCUState { rcuStateThreadCountersV } -> do
    -- Create an MVar the new thread can use to return a result.
    result <- newEmptyMVar

    -- Create a counter for the new thread, and add it to the list.
    threadCounter <- newCounter
    -- Wouldn't <$$> be nice here...
    modifyMVar_ rcuStateThreadCountersV $ return . (threadCounter :)

    -- Spawn the new thread, whose return value goes in @result@.
    tid <- forkIO $ do
      x <- m $ s { rcuStateMyCounter = threadCounter }
      putMVar result x

      -- After the new thread has completed, mark its counter as offline
      -- and remove this counter from the list writers poll.
      writeBarrier
      writeCounter threadCounter offline
      modifyMVar_ rcuStateThreadCountersV $ return . delete threadCounter
    return (RCUThread tid result)
  {-# INLINE forking #-}

  joining (RCUThread _ m) = RCU $ \ _ -> readMVar m
  {-# INLINE joining #-}

  reading (ReadingRCU m) = RCU $ \ s@RCUState { rcuStateMyCounter
                                              , rcuStateGlobalCounter } -> do
    mc <- readCounter rcuStateMyCounter
    -- If this thread was offline, take a snapshot of the global counter so
    -- writers will wait.
    when (mc == offline) $ do
      writeCounter rcuStateMyCounter =<< readCounter rcuStateGlobalCounter   
      -- Make sure that the counter goes online before reads begin.
      storeLoadBarrier
    
    -- Run a read-side critical section.
    x <- m s
    
    -- Announce a quiescent state after the read-side critical section.
    -- TODO: Make this tunable/optional.
    storeLoadBarrier
    writeCounter rcuStateMyCounter =<< readCounter rcuStateGlobalCounter
    storeLoadBarrier
    
    -- Return the result of the read-side critical section.
    return x
  {-# INLINE reading #-}

  writing (WritingRCU m) = RCU $ \ s@RCUState { rcuStateWriterLockV } -> do
    -- Acquire the writer-serializing lock.
    takeMVar rcuStateWriterLockV

    -- Run a write-side critical section.
    x <- m s

    -- Guarantee that writes in this critical section happen before writes in
    -- subsequent critical sections.
    synchronizeIO s

    -- Release the writer-serializing lock.
    putMVar rcuStateWriterLockV ()
    return x
  {-# INLINE writing #-}

instance MonadIO (RCU s) where
  liftIO m = RCU $ \ _ -> m
  {-# INLINE liftIO #-}

-- | Run an RCU computation.
runRCU :: (forall s. RCU s a) -> IO a
runRCU m = do
  unRCU m =<< RCUState <$> newCounter <*> newCounter <*> newMVar [] <*> newMVar ()
{-# INLINE runRCU #-}
