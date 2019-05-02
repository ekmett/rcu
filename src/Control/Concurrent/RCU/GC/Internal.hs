{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK not-home #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2015 Edward Kmett, Paul Khuong and Ted Cooper
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>,
--                Ted Cooper <anthezium@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- QSBR-based RCU
-----------------------------------------------------------------------------
module Control.Concurrent.RCU.GC.Internal
  ( SRef(..)
  , RCUThread(..)
  , RCU(..)
  , runRCU
  , runOnRCU
  , ReadingRCU(..)
  , WritingRCU(..)
  , RCUState(..)
#if BENCHMARKS
  , unRCU
  , runWritingRCU
  , runReadingRCU
  , writeSRefIO
#endif
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.RCU.Class
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Parallel
import Data.Atomics
import Data.IORef
import Data.List
import Data.Primitive
import Prelude hiding (Read(..))
import System.Mem
import qualified Control.Monad.Fail as Fail

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
newtype Counter = Counter (MutableByteArray RealWorld)

instance Eq Counter where
  Counter m == Counter n = sameMutableByteArray m n

newCounter :: Int -> IO Counter
newCounter w = do
  b <- newByteArray 8
  writeByteArray b 0 w
  return (Counter b)
{-# INLINE newCounter #-}

readCounter :: Counter -> IO Int
readCounter (Counter c) = readByteArray c 0
{-# INLINE readCounter #-}

writeCounter :: Counter -> Int -> IO ()
writeCounter (Counter c) w = writeByteArray c 0 w
{-# INLINE writeCounter #-}

incCounter :: Counter -> IO Int
incCounter (Counter c) = do
  x <- fetchAddIntArray c 0 1
  return $! x + 1
{-# INLINE incCounter #-}

newtype Version = Version (IORef ())

newVersion :: IO Version
newVersion = Version <$> newIORef ()

-- | State for an RCU computation.
data RCUState = RCUState
  { -- | Global state
    rcuStateGlobalCounter   :: {-# UNPACK #-} !Counter
  , rcuStateGlobalVersion   :: {-# UNPACK #-} !(IORef Version)
  , rcuStateThreadCountersV :: {-# UNPACK #-} !(MVar [Counter])
  , rcuStateWriterLockV     :: {-# UNPACK #-} !(MVar ())
    -- | Thread state
  , rcuStateMyCounter       :: {-# UNPACK #-} !Counter  -- each thread's state gets its own counter
  , rcuStatePinned          ::                !(Maybe Int)
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
#if !(MIN_VERSION_base(4,13,0))
  fail = Fail.fail
#endif

instance Fail.MonadFail (ReadingRCU s) where
  fail s = ReadingRCU $ \ _ -> Fail.fail s

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
#if !(MIN_VERSION_base(4,13,0))
  fail = Fail.fail
#endif

instance Fail.MonadFail (WritingRCU s) where
  fail s = WritingRCU $ \ _ -> Fail.fail s

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
synchronizeIO s = do
  withMVar (rcuStateThreadCountersV s) $ \ threadCounters -> do
    gc' <- incCounter (rcuStateGlobalCounter s)
    writeCounter (rcuStateMyCounter s) gc'
    let waitForThreads i xxs@(x:xs)
          | i > 2000 = return True
          | otherwise = do
            tc <- readCounter x
            if tc == gc' then waitForThreads (i + 1) xs
            else do
              threadDelay 1
              waitForThreads (i + 1) xxs
        waitForThreads _ [] = return False
    bad <- waitForThreads (0 :: Int) threadCounters
    when bad $ do
      -- slow path
      m <- newEmptyMVar
      stuff s m
      performMinorGC
      sitAndSpin m
  storeLoadBarrier

stuff :: RCUState -> MVar ()  -> IO ()
stuff s m = do
  Version v <- readIORef (rcuStateGlobalVersion s)
  v' <- newVersion
  atomicWriteIORef (rcuStateGlobalVersion s) v'
  _ <- mkWeakIORef v $ putMVar m ()
  return ()
{-# NOINLINE stuff #-}

-- This is awful. It should just takeMVar
sitAndSpin :: MVar () -> IO ()
sitAndSpin m = tryTakeMVar m >>= \case
  Just () -> return ()
  Nothing -> do
    performMajorGC
    sitAndSpin m

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
  forking (RCU m) = RCU $ \ s -> do
    result <- newEmptyMVar
    gc <- readCounter (rcuStateGlobalCounter s)
    threadCounter <- newCounter gc
    modifyMVar_ (rcuStateThreadCountersV s) $ return . (threadCounter :)
    tid <- forkIO $ do
      x <- m $ s { rcuStateMyCounter = threadCounter }
      putMVar result x
      modifyMVar_ (rcuStateThreadCountersV s) $ return . delete threadCounter
    return (RCUThread tid result)
  {-# INLINE forking #-}

  joining (RCUThread _ m) = RCU $ \ _ -> readMVar m
  {-# INLINE joining #-}

  reading (ReadingRCU m) = RCU $ \ s -> do
    v <- readIORef (rcuStateGlobalVersion s)
    x <- m s
    touch v
    writeCounter (rcuStateMyCounter s) =<< readCounter (rcuStateGlobalCounter s)
    return x
  {-# INLINE reading #-}

  writing (WritingRCU m) = RCU $ \ s -> do
    -- Acquire the writer-serializing lock.
    takeMVar (rcuStateWriterLockV s)
    x <- m s
    synchronizeIO s
    putMVar (rcuStateWriterLockV s) ()
    return x
  {-# INLINE writing #-}

instance MonadIO (RCU s) where
  liftIO m = RCU $ \ _ -> m
  {-# INLINE liftIO #-}

-- | Run an RCU computation.
runRCU :: (forall s. RCU s a) -> IO a
runRCU m = do
  v <- newVersion
  unRCU m =<< RCUState <$> newCounter 0
                       <*> newIORef v
                       <*> newMVar []
                       <*> newMVar ()
                       <*> newCounter 0
                       <*> pure Nothing
{-# INLINE runRCU #-}

-- | Run an RCU computation in a thread pinned to a particular core.
runOnRCU :: Int -> (forall s. RCU s a) -> IO a
runOnRCU i m = do
  v <- newVersion
  unRCU m =<< RCUState <$> newCounter 0
                       <*> newIORef v
                       <*> newMVar []
                       <*> newMVar ()
                       <*> newCounter 0
                       <*> pure (Just i)
{-# INLINE runOnRCU #-}
