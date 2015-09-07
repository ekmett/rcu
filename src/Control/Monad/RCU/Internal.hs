{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | STM-based RCU with concurrent writers
module Control.Monad.RCU.Internal
  ( RCURef(..)
  , RCUThread(..)
  , RCU(..)
  , runRCU
  , R(..)
  , W(..)
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.RCU.Class
import Data.Coerce
import Data.Int

--------------------------------------------------------------------------------
-- * Shared References
--------------------------------------------------------------------------------

newtype RCURef s a = RCURef (TVar a)

--------------------------------------------------------------------------------
-- * Read-Side Critical Sections
--------------------------------------------------------------------------------

newtype R s a = R { runR :: IO a } deriving (Functor, Applicative, Monad)

instance MonadNew (R s) where
  type Ref (R s) = RCURef s
  newRef = r where
    r :: forall a. a -> R s (RCURef s a)
    r = coerce (newTVarIO :: a -> IO (TVar a)) 

instance MonadRead (R s) where
  readRef = r where
    r :: forall a. RCURef s a -> R s a
    r = coerce (readTVarIO :: TVar a -> IO a)

--------------------------------------------------------------------------------
-- * Write-Side Critical Sections
--------------------------------------------------------------------------------

newtype W s a = W { runW :: TVar Int64 -> STM a }
  deriving Functor

instance Applicative (W s) where
  pure a = W $ \ _ -> pure a
  W mf <*> W ma = W $ \c -> mf c <*> ma c

instance Monad (W s) where
  return a = W $ \ _ -> pure a
  W m >>= f = W $ \ c -> do
    a <- m c
    runW (f a) c
  fail s = W $ \ _ -> fail s

instance Alternative (W s) where
  empty = W $ \ _ -> empty
  W ma <|> W mb = W $ \c -> ma c <|> mb c

instance MonadPlus (W s) where
  mzero = W $ \ _ -> mzero
  W ma `mplus` W mb = W $ \c -> ma c `mplus` mb c

instance MonadNew (W s) where
  type Ref (W s) = RCURef s
  newRef a = W $ \_ -> RCURef <$> newTVar a

instance MonadRead (W s) where
  readRef (RCURef r) = W $ \ _ -> readTVar r

instance MonadWrite (W s) where
  writeRef (RCURef r) a = W $ \ _ -> writeTVar r a
  synchronize = W $ \ c -> modifyTVar' c (+1) 

--------------------------------------------------------------------------------
-- * RCU Context
--------------------------------------------------------------------------------

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

instance MonadNew (RCU s) where
  type Ref (RCU s) = RCURef s
  newRef a = RCU $ \_ -> RCURef <$> newTVarIO a

-- | For now we don't bother to hold onto the thread id
newtype RCUThread s a = RCUThread { runRCUThread :: MVar a }

instance MonadRCU (RCU s) where
  type ReadT (RCU s) = R s
  type WriteT (RCU s) = W s
  type Thread (RCU s) = RCUThread s
  fork (RCU m) = RCU $ \ c -> do
    result <- newEmptyMVar 
    _ <- forkIO $ do
      x <- m c 
      putMVar result x
    return (RCUThread result)
  join (RCUThread m) = RCU $ \ _ -> readMVar m
  read (R m) = RCU $ \ _ -> m
  write (W m) = RCU $ \ c -> atomically $ do
    _ <- readTVar c -- deliberately incur a data dependency!
    m c

instance MonadIO (RCU s) where
  liftIO m = RCU $ \ _ -> m

runRCU :: (forall s. RCU s a) -> IO a
runRCU m = do
  c <- newTVarIO 0
  unRCU m c
