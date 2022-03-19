{-# LANGUAGE Safe #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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
-----------------------------------------------------------------------------
module Control.Concurrent.RCU.Class
  ( MonadNew(..)
  , MonadReading(..)
  , MonadWriting(..)
  , MonadRCU(..)
  , copySRef
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Prelude
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict

--------------------------------------------------------------------------------
-- * MonadNew
--------------------------------------------------------------------------------

class Monad m => MonadNew s m | m -> s where
  -- | Build a new shared reference
  newSRef :: a -> m (s a)
  default newSRef :: (m ~ t n, MonadTrans t, MonadNew s n) => a -> m (s a)
  newSRef a = lift (newSRef a)

instance MonadNew s m => MonadNew s (ReaderT e m)
instance (MonadNew s m, Monoid w) => MonadNew s (Strict.WriterT w m)
instance (MonadNew s m, Monoid w) => MonadNew s (Lazy.WriterT w m)
instance MonadNew s' m => MonadNew s' (Strict.StateT s m)
instance MonadNew s' m => MonadNew s' (Lazy.StateT s m)
instance (MonadNew s' m, Monoid w) => MonadNew s' (Strict.RWST r w s m)
instance (MonadNew s' m, Monoid w) => MonadNew s' (Lazy.RWST r w s m)
instance MonadNew s m => MonadNew s (ExceptT e m)
instance MonadNew s m => MonadNew s (MaybeT m)
instance MonadNew s m => MonadNew s (IdentityT m)

--------------------------------------------------------------------------------
-- * MonadReading
--------------------------------------------------------------------------------

-- | This is a read-side critical section
class MonadNew s m => MonadReading s m | m -> s where
  -- | Read a shared reference.
  readSRef :: s a -> m a
  default readSRef :: (m ~ t n, MonadTrans t, MonadReading s n) => s a -> m a
  readSRef r = lift (readSRef r)
  {-# INLINE readSRef #-}

-- | Copy a shared reference.
copySRef :: MonadReading s m => s a -> m (s a)
copySRef r = do
  a <- readSRef r
  newSRef a
{-# INLINE copySRef #-}

instance MonadReading s m => MonadReading s (ReaderT e m)
instance (MonadReading s m, Monoid w) => MonadReading s (Strict.WriterT w m)
instance (MonadReading s m, Monoid w) => MonadReading s (Lazy.WriterT w m)
instance MonadReading s' m => MonadReading s' (Strict.StateT s m)
instance MonadReading s' m => MonadReading s' (Lazy.StateT s m)
instance (MonadReading s' m, Monoid w) => MonadReading s' (Strict.RWST r w s m)
instance (MonadReading s' m, Monoid w) => MonadReading s' (Lazy.RWST r w s m)
instance MonadReading s m => MonadReading s (ExceptT e m)
instance MonadReading s m => MonadReading s (MaybeT m)
instance MonadReading s m => MonadReading s (IdentityT m)

--------------------------------------------------------------------------------
-- * MonadWriting
--------------------------------------------------------------------------------

-- | This is a write-side critical section
class MonadReading s m => MonadWriting s m | m -> s where
  -- | Write to a shared reference.
  writeSRef :: s a -> a -> m ()
  default writeSRef :: (m ~ t n, MonadTrans t, MonadWriting s n) => s a -> a -> m ()
  writeSRef r a = lift (writeSRef r a)

  -- | Synchronize with other writers.
  --
  -- No other writer can straddle this time bound. It will either see writes from before, or writes after, but never
  -- some of both!
  synchronize :: m ()
  default synchronize :: (m ~ t n, MonadTrans t, MonadWriting s n) => m ()
  synchronize = lift synchronize

instance MonadWriting s m => MonadWriting s (ReaderT e m)
instance (MonadWriting s m, Monoid w) => MonadWriting s (Strict.WriterT w m)
instance (MonadWriting s m, Monoid w) => MonadWriting s (Lazy.WriterT w m)
instance MonadWriting s' m => MonadWriting s' (Strict.StateT s m)
instance MonadWriting s' m => MonadWriting s' (Lazy.StateT s m)
instance (MonadWriting s' m, Monoid w) => MonadWriting s' (Strict.RWST r w s m)
instance (MonadWriting s' m, Monoid w) => MonadWriting s' (Lazy.RWST r w s m)
instance MonadWriting s m => MonadWriting s (IdentityT m)
instance MonadWriting s m => MonadWriting s (ExceptT e m)
instance MonadWriting s m => MonadWriting s (MaybeT m)

--------------------------------------------------------------------------------
-- * MonadRCU
--------------------------------------------------------------------------------

-- | This is the executor service that can fork, join and execute critical sections.
class
  ( MonadReading s (Reading m)
  , MonadWriting s (Writing m)
  , MonadNew s m
  ) => MonadRCU s m | m -> s where

  -- | A read-side critical section
  type Reading m :: * -> *

  -- | A write-side critical section
  type Writing m :: * -> *

  -- | Threads we can fork and join
  type Thread m :: * -> *

  -- | Fork a thread
  forking :: m a -> m (Thread m a)

  -- | Join a thread
  joining :: Thread m a -> m a

  -- | Run a read-side critical section
  reading :: Reading m a -> m a

  -- | Run a write-side critical section
  writing :: Writing m a -> m a

instance MonadRCU s m => MonadRCU s (ReaderT e m) where
  type Reading (ReaderT e m) = ReaderT e (Reading m)
  type Writing (ReaderT e m) = ReaderT e (Writing m)
  type Thread (ReaderT e m) = Thread m
  forking (ReaderT f) = ReaderT $ \a -> forking (f a)
  joining = lift . joining
  reading (ReaderT f) = ReaderT $ \a -> reading (f a)
  writing (ReaderT f) = ReaderT $ \a -> writing (f a)
  {-# INLINE forking #-}
  {-# INLINE joining #-}
  {-# INLINE reading #-}
  {-# INLINE writing #-}

instance MonadRCU s m => MonadRCU s (IdentityT m) where
  type Reading (IdentityT m) = Reading m
  type Writing (IdentityT m) = Writing m
  type Thread (IdentityT m) = Thread m
  forking (IdentityT m) = IdentityT (forking m)
  joining = lift . joining
  reading m = IdentityT (reading m)
  writing m = IdentityT (writing m)
  {-# INLINE forking #-}
  {-# INLINE joining #-}
  {-# INLINE reading #-}
  {-# INLINE writing #-}

instance MonadRCU s m => MonadRCU s (ExceptT e m) where
  type Reading (ExceptT e m) = ExceptT e (Reading m)
  type Writing (ExceptT e m) = ExceptT e (Writing m)
  type Thread (ExceptT e m) = ExceptT e (Thread m)
  forking (ExceptT m) = lift $ ExceptT <$> forking m
  joining (ExceptT m) = ExceptT $ joining m
  reading (ExceptT m) = ExceptT $ reading m
  writing (ExceptT m) = ExceptT $ writing m
  {-# INLINE forking #-}
  {-# INLINE joining #-}
  {-# INLINE reading #-}
  {-# INLINE writing #-}

instance MonadRCU s m => MonadRCU s (MaybeT m) where
  type Reading (MaybeT m) = MaybeT (Reading m)
  type Writing (MaybeT m) = MaybeT (Writing m)
  type Thread (MaybeT m) = MaybeT (Thread m)
  forking (MaybeT m) = lift $ MaybeT <$> forking m
  joining (MaybeT m) = MaybeT $ joining m
  reading (MaybeT m) = MaybeT $ reading m
  writing (MaybeT m) = MaybeT $ writing m
  {-# INLINE forking #-}
  {-# INLINE joining #-}
  {-# INLINE reading #-}
  {-# INLINE writing #-}

instance (MonadRCU s m, Monoid e) => MonadRCU s (Strict.WriterT e m) where
  type Reading (Strict.WriterT e m) = Strict.WriterT e (Reading m)
  type Writing (Strict.WriterT e m) = Strict.WriterT e (Writing m)
  type Thread (Strict.WriterT e m) = Strict.WriterT e (Thread m)
  forking (Strict.WriterT m) = lift $ Strict.WriterT <$> forking m
  joining (Strict.WriterT m) = Strict.WriterT $ joining m
  reading (Strict.WriterT m) = Strict.WriterT $ reading m
  writing (Strict.WriterT m) = Strict.WriterT $ writing m
  {-# INLINE forking #-}
  {-# INLINE joining #-}
  {-# INLINE reading #-}
  {-# INLINE writing #-}

instance (MonadRCU s m, Monoid e) => MonadRCU s (Lazy.WriterT e m) where
  type Reading (Lazy.WriterT e m) = Lazy.WriterT e (Reading m)
  type Writing (Lazy.WriterT e m) = Lazy.WriterT e (Writing m)
  type Thread (Lazy.WriterT e m) = Lazy.WriterT e (Thread m)
  forking (Lazy.WriterT m) = lift $ Lazy.WriterT <$> forking m
  joining (Lazy.WriterT m) = Lazy.WriterT $ joining m
  reading (Lazy.WriterT m) = Lazy.WriterT $ reading m
  writing (Lazy.WriterT m) = Lazy.WriterT $ writing m
  {-# INLINE forking #-}
  {-# INLINE joining #-}
  {-# INLINE reading #-}
  {-# INLINE writing #-}
