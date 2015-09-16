{-# LANGUAGE Trustworthy #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2015 Edward Kmett and Ted Cooper
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
--                Ted Cooper <anthezium@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Quiescent-State-Based Reclamation
-----------------------------------------------------------------------------
module Control.Concurrent.RCU.QSBR
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
import Control.Concurrent.RCU.QSBR.Internal
