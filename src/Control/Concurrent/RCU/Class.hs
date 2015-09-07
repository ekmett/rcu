{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}

module Control.Concurrent.RCU.Class
  ( MonadNew(..)
  , MonadRead(..)
  , MonadWrite(..)
  , MonadRCU(..)
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import Prelude hiding (read)

class Monad m => MonadNew m where
  type Ref m :: * -> *

  newRef :: a -> m (Ref m a)
  default newRef :: (m ~ t n, MonadTrans t, MonadNew n, Ref n ~ Ref m) => a -> m (Ref m a)
  newRef a = lift (newRef a)

instance MonadNew m => MonadNew (ReaderT e m) where
  type Ref (ReaderT e m) = Ref m

instance (MonadNew m, Monoid w) => MonadNew (Strict.WriterT w m) where
  type Ref (Strict.WriterT w m) = Ref m

instance (MonadNew m, Monoid w) => MonadNew (Lazy.WriterT w m) where
  type Ref (Lazy.WriterT w m) = Ref m

instance MonadNew m => MonadNew (Strict.StateT s m) where
  type Ref (Strict.StateT s m) = Ref m

instance MonadNew m => MonadNew (Lazy.StateT s m) where
  type Ref (Lazy.StateT s m) = Ref m

instance (MonadNew m, Monoid w) => MonadNew (Strict.RWST r w s m) where
  type Ref (Strict.RWST r w s m) = Ref m

instance (MonadNew m, Monoid w) => MonadNew (Lazy.RWST r w s m) where
  type Ref (Lazy.RWST r w s m) = Ref m

instance MonadNew m => MonadNew (ExceptT e m) where
  type Ref (ExceptT e m) = Ref m

instance MonadNew m => MonadNew (MaybeT m) where
  type Ref (MaybeT m) = Ref m

instance MonadNew m => MonadNew (IdentityT m) where
  type Ref (IdentityT m) = Ref m

-- | This is a read-side critical section
class MonadNew m => MonadRead m where
  readRef :: Ref m a -> m a
  default readRef :: (m ~ t n, MonadTrans t, MonadRead n, Ref n ~ Ref m) => Ref m a -> m a
  readRef r = lift (readRef r)

instance MonadRead m => MonadRead (ReaderT e m)
instance (MonadRead m, Monoid w) => MonadRead (Strict.WriterT w m)
instance (MonadRead m, Monoid w) => MonadRead (Lazy.WriterT w m)
instance MonadRead m => MonadRead (Strict.StateT s m)
instance MonadRead m => MonadRead (Lazy.StateT s m)
instance (MonadRead m, Monoid w) => MonadRead (Strict.RWST r w s m)
instance (MonadRead m, Monoid w) => MonadRead (Lazy.RWST r w s m)
instance MonadRead m => MonadRead (ExceptT e m)
instance MonadRead m => MonadRead (MaybeT m)
instance MonadRead m => MonadRead (IdentityT m)

-- | This is a write-side critical section
class MonadRead m => MonadWrite m where
  writeRef :: Ref m a -> a -> m ()
  default writeRef :: (m ~ t n, MonadTrans t, MonadWrite n, Ref n ~ Ref m) => Ref m a -> a -> m ()
  writeRef r a = lift (writeRef r a)

  synchronize :: m ()
  default synchronize :: (m ~ t n, MonadTrans t, MonadWrite n, Ref n ~ Ref m) => m ()
  synchronize = lift synchronize

instance MonadWrite m => MonadWrite (ReaderT e m)
instance (MonadWrite m, Monoid w) => MonadWrite (Strict.WriterT w m)
instance (MonadWrite m, Monoid w) => MonadWrite (Lazy.WriterT w m)
instance MonadWrite m => MonadWrite (Strict.StateT s m)
instance MonadWrite m => MonadWrite (Lazy.StateT s m)
instance (MonadWrite m, Monoid w) => MonadWrite (Strict.RWST r w s m)
instance (MonadWrite m, Monoid w) => MonadWrite (Lazy.RWST r w s m)
instance MonadWrite m => MonadWrite (IdentityT m)
instance MonadWrite m => MonadWrite (ExceptT e m)
instance MonadWrite m => MonadWrite (MaybeT m)

-- | This is the executor service that can fork, join and execute critical sections.
class (MonadRead (ReadT m), MonadWrite (WriteT m), MonadNew m) => MonadRCU m where
  type ReadT m :: * -> *
  type WriteT m :: * -> *
  type Thread m :: * -> *
  fork  :: m a -> m (Thread m a)
  join  :: Thread m a -> m a
  read  :: ReadT m a -> m a
  write :: WriteT m a -> m a

instance MonadRCU m => MonadRCU (ReaderT e m) where
  type ReadT (ReaderT e m) = ReaderT e (ReadT m)
  type WriteT (ReaderT e m) = ReaderT e (WriteT m)
  type Thread (ReaderT e m) = Thread m
  fork (ReaderT f)  = ReaderT $ \a -> fork (f a)
  join              = lift . join
  read (ReaderT f)  = ReaderT $ \a -> read (f a)
  write (ReaderT f) = ReaderT $ \ a -> write (f a)

instance MonadRCU m => MonadRCU (IdentityT m) where
  type ReadT (IdentityT m)  = ReadT m
  type WriteT (IdentityT m) = WriteT m
  type Thread (IdentityT m) = Thread m
  fork (IdentityT m) = IdentityT (fork m)
  join    = lift . join
  read m  = IdentityT (read m)
  write m = IdentityT (write m)

instance MonadRCU m => MonadRCU (ExceptT e m) where
  type ReadT (ExceptT e m)  = ExceptT e (ReadT m)
  type WriteT (ExceptT e m) = ExceptT e (WriteT m)
  type Thread (ExceptT e m) = ExceptT e (Thread m)
  fork (ExceptT m)  = lift $ ExceptT <$> fork m
  join (ExceptT m)  = ExceptT $ join m
  read (ExceptT m)  = ExceptT $ read m
  write (ExceptT m) = ExceptT $ write m

instance MonadRCU m => MonadRCU (MaybeT m) where
  type ReadT (MaybeT m)  = MaybeT (ReadT m)
  type WriteT (MaybeT m) = MaybeT (WriteT m)
  type Thread (MaybeT m) = MaybeT (Thread m)
  fork (MaybeT m)  = lift $ MaybeT <$> fork m
  join (MaybeT m)  = MaybeT $ join m
  read (MaybeT m)  = MaybeT $ read m
  write (MaybeT m) = MaybeT $ write m

instance (MonadRCU m, Monoid e) => MonadRCU (Strict.WriterT e m) where
  type ReadT (Strict.WriterT e m)  = Strict.WriterT e (ReadT m)
  type WriteT (Strict.WriterT e m) = Strict.WriterT e (WriteT m)
  type Thread (Strict.WriterT e m) = Strict.WriterT e (Thread m)
  fork (Strict.WriterT m)  = lift $ Strict.WriterT <$> fork m
  join (Strict.WriterT m)  = Strict.WriterT $ join m
  read (Strict.WriterT m)  = Strict.WriterT $ read m
  write (Strict.WriterT m) = Strict.WriterT $ write m

instance (MonadRCU m, Monoid e) => MonadRCU (Lazy.WriterT e m) where
  type ReadT (Lazy.WriterT e m)  = Lazy.WriterT e (ReadT m)
  type WriteT (Lazy.WriterT e m) = Lazy.WriterT e (WriteT m)
  type Thread (Lazy.WriterT e m) = Lazy.WriterT e (Thread m)
  fork (Lazy.WriterT m) = lift $ Lazy.WriterT <$> fork m
  join (Lazy.WriterT m)  = Lazy.WriterT $ join m
  read (Lazy.WriterT m)  = Lazy.WriterT $ read m
  write (Lazy.WriterT m) = Lazy.WriterT $ write m
