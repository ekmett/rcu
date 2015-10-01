{-# LANGUAGE Trustworthy #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2015 Edward Kmett and Paul Khuong
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
--                Ted Cooper <anthezium@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Unfenched QSBR w/ Finalizer-Based Fallback Reclamation
-----------------------------------------------------------------------------
module Control.Concurrent.RCU.GC
  ( SRef
  , RCU, runRCU
  , MonadNew(..)
  , MonadReading(..)
  , MonadWriting(..)
  , MonadRCU(..)
  -- * Implementation Details
  , ReadingRCU
  , WritingRCU
  , RCUThread(rcuThreadId)
  ) where

import Control.Concurrent.RCU.Class
import Control.Concurrent.RCU.GC.Internal
