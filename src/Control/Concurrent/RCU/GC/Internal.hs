{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
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
-- Copyright   :  (C) 2015 Edward Kmett and Paul Khuong
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>, 
--                Ted Cooper <anthezium@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- GC-based RCU
-----------------------------------------------------------------------------
module Control.Concurrent.RCU.GC.Internal
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
import Control.Monad.Primitive
import Control.Parallel
import Data.Atomics 
import Data.IORef 
import Data.Primitive
import Prelude hiding (read, Read)
import System.Mem
import System.Mem.Weak

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

newtype Version = Version (IORef ())

newVersion :: IO Version
newVersion = Version <$> newIORef ()

-- | State for an RCU computation.
data RCUState = RCUState
  { rcuStateGlobalCounter   :: {-# UNPACK #-} !(IORef Version)
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

stuff :: RCUState -> MVar ()  -> IO ()
stuff s m = do
  Version v <- readIORef (rcuStateGlobalCounter s)
  v' <- newVersion
  writeIORef (rcuStateGlobalCounter s) v' 
  storeLoadBarrier
  _ <- mkWeakIORef v $ putMVar m ()
  return ()
{-# NOINLINE stuff #-}

synchronizeIO :: RCUState -> IO () 
synchronizeIO s = do
  m <- newEmptyMVar
  stuff s m
  performMajorGC
  sitAndSpin m

-- This is awful. It should just takeMVar
sitAndSpin :: MVar () -> IO ()
sitAndSpin m = tryTakeMVar m >>= \case
  Just () -> return ()
  Nothing -> do
    performMajorGC
    -- ba <- newByteArray 2000000
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
    -- Create an MVar the new thread can use to return a result.
    result <- newEmptyMVar
    tid <- forkIO $ do
      x <- m s
      putMVar result x
    return (RCUThread tid result)
  {-# INLINE forking #-}

  joining (RCUThread _ m) = RCU $ \ _ -> readMVar m
  {-# INLINE joining #-}

  reading (ReadingRCU m) = RCU $ \ s -> do 
    v <- readIORef (rcuStateGlobalCounter s)
    loadLoadBarrier
    x <- m s
    touch v
    return x
  {-# INLINE reading #-}

  writing (WritingRCU m) = RCU $ \ s -> do
    takeMVar (rcuStateWriterLockV s)
    x <- m s
    -- Guarantee that writes in this critical section happen before writes in
    -- subsequent critical sections.
    synchronizeIO s
    -- Release the writer-serializing lock.
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
  rv <- newIORef v
  l <- newMVar ()
  unRCU m (RCUState rv l)
{-# INLINE runRCU #-}
