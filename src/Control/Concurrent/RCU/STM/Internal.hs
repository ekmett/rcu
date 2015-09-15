{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
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
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
--                Ted Cooper <anthezium@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- STM-based RCU with concurrent writers
-----------------------------------------------------------------------------
module Control.Concurrent.RCU.STM.Internal
  ( SRef(..)
  , RCUThread(..)
  , RCU(..)
  , runRCU
  , ReadingRCU(..)
  , WritingRCU(..)
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.RCU.Class
import Control.Monad
import Control.Monad.IO.Class
import Data.Coerce
import Data.Int
import Prelude hiding (read, Read)

--------------------------------------------------------------------------------
-- * Shared References
--------------------------------------------------------------------------------

-- | Shared references
newtype SRef s a = SRef { unSRef :: TVar a }
  deriving Eq

--------------------------------------------------------------------------------
-- * Read-Side Critical Sections
--------------------------------------------------------------------------------

-- | This is the basic read-side critical section for an RCU computation
newtype ReadingRCU s a = ReadingRCU { runReadingRCU :: IO a } deriving (Functor, Applicative, Monad)

instance MonadNew (SRef s) (ReadingRCU s) where
  newSRef = r where
    r :: forall a. a -> ReadingRCU s (SRef s a)
    r = coerce (newTVarIO :: a -> IO (TVar a))

instance MonadReading (SRef s) (ReadingRCU s) where
  readSRef = r where
    r :: forall a. SRef s a -> ReadingRCU s a
    r = coerce (readTVarIO :: TVar a -> IO a)
  {-# INLINE readSRef #-}

--------------------------------------------------------------------------------
-- * Write-Side Critical Sections
--------------------------------------------------------------------------------

-- | This is the basic write-side critical section for an RCU computation
newtype WritingRCU s a = WritingRCU { runWritingRCU :: TVar Int64 -> STM a }
  deriving Functor

instance Applicative (WritingRCU s) where
  pure a = WritingRCU $ \ _ -> pure a
  WritingRCU mf <*> WritingRCU ma = WritingRCU $ \c -> mf c <*> ma c

instance Monad (WritingRCU s) where
  return a = WritingRCU $ \ _ -> pure a
  WritingRCU m >>= f = WritingRCU $ \ c -> do
    a <- m c
    runWritingRCU (f a) c
  fail s = WritingRCU $ \ _ -> fail s

instance Alternative (WritingRCU s) where
  empty = WritingRCU $ \ _ -> empty
  WritingRCU ma <|> WritingRCU mb = WritingRCU $ \c -> ma c <|> mb c

instance MonadPlus (WritingRCU s) where
  mzero = WritingRCU $ \ _ -> mzero
  WritingRCU ma `mplus` WritingRCU mb = WritingRCU $ \c -> ma c `mplus` mb c

instance MonadNew (SRef s) (WritingRCU s) where
  newSRef a = WritingRCU $ \_ -> SRef <$> newTVar a

instance MonadReading (SRef s) (WritingRCU s) where
  readSRef (SRef r) = WritingRCU $ \ _ -> readTVar r
  {-# INLINE readSRef #-}

instance MonadWriting (SRef s) (WritingRCU s) where
  writeSRef (SRef r) a = WritingRCU $ \ _ -> writeTVar r a
  synchronize = WritingRCU $ \ c -> modifyTVar' c (+1)

--------------------------------------------------------------------------------
-- * RCU Context
--------------------------------------------------------------------------------

-- | This is an RCU computation. It can use 'forking' and 'joining' to form
-- new threads, and then you can use 'reading' and 'writing' to run classic
-- read-side and write-side RCU computations. Contention between multiple
-- write-side computations is managed by STM.
newtype RCU s a = RCU { unRCU :: TVar Int64 -> IO a }
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
  newSRef a = RCU $ \_ -> SRef <$> newTVarIO a

-- | This is a basic 'RCU' thread. It may be embellished when running in a more
-- exotic context.
data RCUThread s a = RCUThread
  { rcuThreadId :: {-# UNPACK #-} !ThreadId
  , rcuThreadVar :: {-# UNPACK #-} !(MVar a)
  }

instance MonadRCU (SRef s) (RCU s) where
  type Reading (RCU s) = ReadingRCU s
  type Writing (RCU s) = WritingRCU s
  type Thread (RCU s) = RCUThread s
  forking (RCU m) = RCU $ \ c -> do
    result <- newEmptyMVar
    tid <- forkIO $ do
      x <- m c
      putMVar result x
    return (RCUThread tid result)
  joining (RCUThread _ m) = RCU $ \ _ -> readMVar m
  reading (ReadingRCU m) = RCU $ \ _ -> m
  writing (WritingRCU m) = RCU $ \ c -> atomically $ do
    _ <- readTVar c -- deliberately incur a data dependency!
    m c
  {-# INLINE forking #-}
  {-# INLINE joining #-}
  {-# INLINE reading #-}
  {-# INLINE writing #-}

instance MonadIO (RCU s) where
  liftIO m = RCU $ \ _ -> m
  {-# INLINE liftIO #-}

-- | Run an RCU computation.
runRCU :: (forall s. RCU s a) -> IO a
runRCU m = do
  c <- newTVarIO 0
  unRCU m c
{-# INLINE runRCU #-}
