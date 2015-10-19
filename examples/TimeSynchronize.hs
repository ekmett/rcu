{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Concurrent
import Control.Concurrent.RCU.MODE
import Control.Concurrent.RCU.Class
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM)
import Data.Bits ((.&.), (.|.), shiftL)
import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.Word (Word64)
import Options.Applicative (auto, execParser, fullDesc, help, info, long, metavar, option, progDesc, short)
import Prelude hiding (read)
import System.CPUTime.Rdtsc

data List s a = Nil | Cons a (SRef s (List s a))

-- | Checks whether the list contains the same bit twice.
--   Does not short-circuit, so all read-side critical sections
--   have similar memory access patterns and durations.
checkForDup :: MonadReading (SRef s) (m s) => (Word64, Bool) -> List s Word64 -> m s Bool
checkForDup (_,   dup) Nil         = return dup
checkForDup (acc, dup) (Cons x rn) = checkForDup (acc .|. x, dup || acc .&. x /= 0) =<< readSRef rn

--snapshot :: MonadReading (SRef s) (m s) => [a] -> List s a -> m s [a]
--snapshot acc Nil         = return $ reverse acc
--snapshot acc (Cons x rn) = snapshot (x : acc) =<< readSRef rn

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

writer :: Integer -> WritingRCU s Word64 -> Word64 -> WritingRCU s Word64
writer 0  _ !acc = return acc
writer !n m !acc = do d <- m
                      writer (pred n) m (acc + d)
                     

moveDback :: SRef s (List s a) -> WritingRCU s Word64
moveDback rl = WritingRCU $ \ s -> do
  (rc, ne) <- flip runWritingRCU s $ do
    Cons _a rb <- readSRef rl
    Cons _b rc <- readSRef rb
    -- duplicate pointer to B
    rb'       <- copySRef rb
    Cons c rd <- readSRef rc
    ne        <- readSRef rd
    -- link in a new C after A
    writeSRef rb $ Cons c rb'
    return (rc, ne)
#if MEASURE_SYNCHRONIZE
  beg <- rdtsc
#endif
  -- any reader who starts during this grace period 
  -- sees either "ABCDE" or "ACBCDE"
  runWritingRCU synchronize s
#if MEASURE_SYNCHRONIZE
  end <- rdtsc
#else
  let beg = 0
      end = 0
#endif
  -- unlink the old C
  flip runWritingRCU s $ do writeSRef rc ne
                            return $ end - beg

testList :: RCU s (SRef s (List s Word64))
testList = helper 4 =<< newSRef Nil
  where helper (- 1) tl = return tl
        helper i     tl = helper (pred i) =<< newSRef (Cons (shiftL 1 i) tl)

data Opts = Opts { nReserved :: Integer 
                 , nUpdates  :: Integer }

main :: IO ()
main = do 
  Opts { nReserved, nUpdates } <- execParser opts
  nCaps <- getNumCapabilities
  putStrLn $ "nCaps: " ++ show nCaps
  let nReaders = fromIntegral nCaps - nReserved
  putStrLn $ "reserved: " ++ show nReserved ++ ", readers: " ++ show nReaders ++ ", updates: " ++ show nUpdates
  let nTotal = fromIntegral $ nUpdates * nReaders :: Double
  (ods, wfrd, wd, wfd) <- runOnRCU 0 $ RCU $ \ s -> do
    -- initialize list
    hd  <- unRCU testList s
    -- initialize flag writer uses to stop readers
    rf  <- unRCU (newSRef False) s
    -- spawn nReaders readers, each takes snapshots of the list until the writer has finished
    rts <- forM [2..fromIntegral nReaders + 1] $ \ i -> flip unRCU (s { rcuStatePinned = Just i }) $ forking $ reading $ ReadingRCU 
         $ \ s -> do beg <- rdtsc
                     x   <- evaluate . force =<< unRCU (reader rf 0 hd) s -- how long do the snapshots take?
                     mid <- rdtsc
                     _   <- evaluate . force $ x -- how long does walking over the result take?
                     end <- rdtsc
                     return ( x
                            , mid - beg
                            , end - mid )
    -- spawn a writer to move a node from a later position to an earlier position nUpdates times
    wt  <- flip unRCU (s { rcuStatePinned = Just 1 }) $ forking $ writing $ WritingRCU 
         $ \ s -> do beg <- rdtsc
                     x   <- evaluate . force =<< runWritingRCU (writer nUpdates (moveDback hd) 0) s
                     runWritingRCU (writeSRef rf True) s
                     mid <- rdtsc
                     _   <- evaluate . force $ x
                     end <- rdtsc
                     return ( fromIntegral x / fromIntegral nUpdates :: Double
                            , mid - beg
                            , end - mid )
    -- wait for the readers to finish
    ods <- mapM (\ rt -> unRCU (joining rt) s) rts
    -- wait for the writer to finish
    (wfrd :: Double, wd, wfd) <- unRCU (joining wt) s
    return (ods, wfrd, wd, wfd)
  let outs = map a ods
      rds  = map b ods :: [Word64]
      rfds = map c ods :: [Word64]
      a (x,_,_) = x
      b (_,y,_) = y
      c (_,_,z) = z
  putStrLn $ "dups by thread:" ++ (intercalate ", " $ zipWith (\ i dups -> show i ++ ": " ++ show dups) [(1 :: Integer)..] outs)
  putStrLn $ "average dups per update: " ++ show (fromIntegral (sum outs) / nTotal)
  putStrLn $ "reader times: " ++ show rds
  putStrLn $ "reader evaluate . force times: " ++ show rfds
  putStrLn $ "writer time: " ++ show wd
  putStrLn $ "writer evaluate . force time: " ++ show wfd
  let aud = fromIntegral (wd - wfd) / fromIntegral nUpdates :: Double
  putStrLn $ "average writer update time: " ++ show aud
#if MEASURE_SYNCHRONIZE
  putStrLn $ "average synchronize time: " ++ show wfrd
#endif
  where opts = info optsParser
             ( fullDesc
            <> progDesc "Measure writer latency for synchronize." )
        optsParser = Opts <$> nReservedParser <*> nUpdatesParser
        nReservedParser = option auto
                        ( long "reserved"
                       <> short 'r'
                       <> metavar "N"
                       <> help "N OS threads reserved for other stuff" )
        nUpdatesParser = option auto
                       ( long "updates"
                      <> short 'u'
                      <> metavar "M"
                      <> help "Writer thread performs M updates" )
        
