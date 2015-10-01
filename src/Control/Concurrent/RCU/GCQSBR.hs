{-# LANGUAGE Trustworthy #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2015 Edward Kmett and Paul Khuong
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Mixed Finalizer-based and Quiescent-State-Based Reclamation
-----------------------------------------------------------------------------
module Control.Concurrent.RCU.GCQSBR
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
import Control.Concurrent.RCU.GCQSBR.Internal
