{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2015 Edward Kmett and Ted Cooper
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>,
--                Ted Cooper <anthezium@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Which counter increment is faster?
-----------------------------------------------------------------------------
module Main where

import Control.Monad (forM_)
import Control.Monad.Primitive (primitive)
import Criterion.Main (bench, bgroup, defaultMain, nfIO)
import Data.Word (Word64)
import Data.Primitive
import GHC.Exts --(MutableByteArray, plusWord, readWord64Array, writeWord64Array)
import GHC.Word (Word64 (W64#))

-- | Counter for causal ordering.
newtype Counter = Counter (MutableByteArray RealWorld)

instance Eq Counter where
  Counter m == Counter n = sameMutableByteArray m n

offline :: Word64
offline = 0

online :: Word64
online  = 1

-- counterInc :: Word64
-- counterInc = 2 -- online threads will never overflow to 0

newCounter :: IO Counter
newCounter = do
  b <- newByteArray 8
  writeByteArray b 0 online
  return (Counter b)
{-# INLINE newCounter #-}

readCounter :: Counter -> IO Word64
readCounter (Counter c) = readByteArray c 0
{-# INLINE readCounter #-}

writeCounter :: Counter -> Word64 -> IO ()
writeCounter (Counter c) w = writeByteArray c 0 w
{-# INLINE writeCounter #-}

incCounterAtomic :: Counter -> IO Word64
incCounterAtomic (Counter (MutableByteArray c)) = primitive $ \ s ->
  case fetchAddIntArray# c 0# 2# s of
       (# s', r #) ->
#if MIN_VERSION_base(4,17,0)
         (# s', W64# (wordToWord64# (int2Word# r)) #)
#else
         (# s', W64# (int2Word# r) #)
#endif
{-# INLINE incCounterAtomic #-}

incCounterNonAtomicFancy :: Counter -> IO Word64
incCounterNonAtomicFancy (Counter (MutableByteArray c)) = primitive $ \ s ->
  case readWord64Array# c 0# s of
       (# s', r #) ->
#if MIN_VERSION_base(4,17,0)
         case plusWord64# r (wordToWord64# 2##) of
#else
         case plusWord# r 2## of
#endif
              r' -> case writeWord64Array# c 0# r' s' of
                         s'' -> (# s'', W64# r' #)
{-# INLINE incCounterNonAtomicFancy #-}

incCounterNonAtomic :: Counter -> IO Word64
incCounterNonAtomic c = do
  x <- (+ 2) <$> readCounter c
  writeCounter c x
  return x
{-# INLINE incCounterNonAtomic #-}

main :: IO ()
main = defaultMain [ bgroup "incCounterAtomic"         $ bunches incCounterAtomic
                   , bgroup "incCounterNonAtomicFancy" $ bunches incCounterNonAtomicFancy
                   , bgroup "incCounterNonAtomic"      $ bunches incCounterNonAtomic ]
  where bunches m = [ bench (show n)
                    $ nfIO $ do c <- newCounter
                                forM_ [1..n] $ \ _ -> m c
                    | n <- map ((10 :: Word64) ^) [(6 :: Word64)..7] ]

