{-# LANGUAGE Trustworthy #-}
module Control.Concurrent.RCU.Class
  ( SRef
  , MonadNew(..)
  , MonadReading(..)
  , MonadWriting(..)
  , MonadRCU(..)
  ) where

import Control.Concurrent.RCU.Internal
