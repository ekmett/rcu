{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
import Control.Concurrent.RCU.QSBR
import Control.Concurrent.RCU.Class
import Control.Monad (forM, forM_, replicateM, replicateM_, when)
import Data.Bits ((.&.), (.|.), shiftL)
import Data.List (intercalate)
import Data.Word (Word64)
import Debug.Trace (trace)
import Prelude hiding (read)

data List s a = Nil | Cons a (SRef s (List s a))

-- | Checks whether the list contains the same bit twice.
--   Does not short-circuit, so all read-side critical sections
--   have similar memory access patterns and durations.
checkForDup :: MonadReading (SRef s) (m s) => (Word64, Bool) -> List s Word64 -> m s Bool
checkForDup (_,   dup) Nil         = return dup
checkForDup (acc, dup) (Cons x rn) = checkForDup (acc .|. x, dup || acc .&. x /= 0) =<< readSRef rn


snapshot :: MonadReading (SRef s) (m s) => [a] -> List s a -> m s [a]
snapshot acc Nil         = return $ reverse acc
snapshot acc (Cons x rn) = snapshot (x : acc) =<< readSRef rn

reader :: SRef s Bool -> Integer -> SRef s (List s Word64) -> RCU s Integer
reader rf !acc hd = do
     (acc', flag) <- reading $ do dup  <- checkForDup (0, False) =<< readSRef hd 
                                  --when dup $ do
                                  --  xs <- snapshot [] =<< readSRef hd
                                  --  trace (show xs) $ return ()
                                  flag <- readSRef rf
                                  return (if dup then succ acc else acc, flag)
     if flag
        then return acc'
        else reader rf acc' hd

moveDback :: SRef s (List s a) -> WritingRCU s ()
moveDback rl = do
  Cons _a rb <- readSRef rl
  Cons _b rc <- readSRef rb
  -- duplicate pointer to B
  rb'       <- copySRef rb
  Cons c rd <- readSRef rc
  ne        <- readSRef rd
  -- link in a new C after A
  writeSRef rb $ Cons c rb'
  -- any reader who starts during this grace period 
  -- sees either "ABCDE" or "ACBCDE"
  synchronize
  -- unlink the old C
  writeSRef rc ne

testList :: RCU s (SRef s (List s Word64))
testList = helper 4 =<< newSRef Nil
  where helper (- 1) tl = return tl
        helper i     tl = helper (pred i) =<< newSRef (Cons (shiftL 1 i) tl)

main :: IO ()
main = do 
  let nReaders = 7
      nUpdates = 3200000
      nTotal   = fromIntegral $ nUpdates * nReaders :: Double
  outs <- runRCU $ do
    -- initialize list
    hd <- testList
    -- initialize flag writer uses to stop readers
    rf <- newSRef False
    -- spawn nReaders readers, each takes snapshots of the list until the writer has finished
    rts <- replicateM nReaders $ forking $ reader rf 0 hd
    -- spawn a writer to move a node from a later position to an earlier position nUpdates times
    wt  <- forking $ writing $ do replicateM_ nUpdates $ moveDback hd
                                  writeSRef rf True
    
    -- wait for the readers to finish
    outs <- forM rts joining
    -- wait for the writer to finish
    joining wt
    return outs
  putStrLn ("dups by thread:" ++ (intercalate ", " $ zipWith (\ i dups -> show i ++ ": " ++ show dups) [(1 :: Integer)..] outs))
  putStrLn $ "average dups per update: " ++ show (fromIntegral (sum outs) / nTotal)
