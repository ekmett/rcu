{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Concurrent
import Control.Concurrent.RCU.MODE.Internal
import Control.Concurrent.RCU.Class
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad
import Data.Bits ((.&.), (.|.), shiftL)
import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.Word (Word64)
import Options.Applicative (auto, execParser, fullDesc, help, info, long, metavar, option, progDesc, short)
import Prelude hiding (read)

#if UNBOUND
import Data.Time (UTCTime(..), Day(..), NominalDiffTime, diffUTCTime, getCurrentTime)

type TimeT = NominalDiffTime

timer :: IO UTCTime
timer = getCurrentTime

timerDiff :: UTCTime -> UTCTime -> TimeT
timerDiff = diffUTCTime

timeZero :: TimeT
timeZero = let t = UTCTime (ModifiedJulianDay 0) 0 in t `diffUTCTime` t
#else
import System.CPUTime.Rdtsc (rdtsc)

type TimeT = Word64

timer :: IO Word64
timer = rdtsc

timerDiff :: TimeT -> TimeT -> TimeT
timerDiff = (-)

timeZero :: TimeT
timeZero = 0
#endif

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

writer :: Int -> WritingRCU s TimeT -> WritingRCU s TimeT
writer n m = do
#if MEASURE_SYNCHRONIZE
  helper n m timeZero
  where helper 0  _ !acc = return acc
        helper !n m !acc = do d <- m
                              helper (pred n) m (acc + d)
#else
  replicateM_ n m
  return timeZero
#endif


moveDback :: SRef s (List s a) -> WritingRCU s TimeT
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
  beg <- timer
#endif
  -- any reader who starts during this grace period
  -- sees either "ABCDE" or "ACBCDE"
  runWritingRCU synchronize s
#if MEASURE_SYNCHRONIZE
  end <- timer
  let d = end `timerDiff` beg
#else
  let d = timeZero
#endif
  -- unlink the old C
  flip runWritingRCU s $ do writeSRef rc ne
                            return d

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
         $ \ s' -> do beg <- timer
                      x   <- evaluate . force =<< unRCU (reader rf 0 hd) s' -- how long do the snapshots take?
                      mid <- timer
                      _   <- evaluate . force $ x -- how long does walking over the result take?
                      end <- timer
                      return ( x
                             , mid `timerDiff` beg
                             , end `timerDiff` mid )
    -- spawn a writer to move a node from a later position to an earlier position nUpdates times
    wt  <- flip unRCU (s { rcuStatePinned = Just 1 }) $ forking $ writing $ WritingRCU
         $ \ s' -> do beg <- timer
                      x   <- evaluate . force =<< runWritingRCU (writer (fromIntegral nUpdates) (moveDback hd)) s'
                      runWritingRCU (writeSRef rf True) s'
                      mid <- timer
                      _   <- evaluate . force $ x
                      end <- timer
#if UNBOUND
                      let d = x
#else
                      let d = fromIntegral x :: Double
#endif
                      return ( d / fromIntegral nUpdates
                             , mid `timerDiff` beg
                             , end `timerDiff` mid )
    -- wait for the readers to finish
    ods <- mapM (\ rt -> unRCU (joining rt) s) rts
    -- wait for the writer to finish
    (wfrd, wd, wfd) <- unRCU (joining wt) s
    return (ods, wfrd, wd, wfd)
  let outs = map a ods
      rds  = map b ods :: [TimeT]
      rfds = map c ods :: [TimeT]
      a (x,_,_) = x
      b (_,y,_) = y
      c (_,_,z) = z
  putStrLn $ "dups by thread:" ++ intercalate ", " (zipWith (\ i dups -> show i ++ ": " ++ show dups) [(1 :: Integer)..] outs)
  putStrLn $ "average dups per update: " ++ show (fromIntegral (sum outs) / nTotal)
#if UNBOUND
  putStrLn   "times in SECONDS"
#else
  putStrLn   "times in TICKS"
#endif
  putStrLn $ "reader times: " ++ show rds
  putStrLn $ "reader evaluate . force times: " ++ show rfds
  putStrLn $ "writer time: " ++ show wd
  putStrLn $ "writer evaluate . force time: " ++ show wfd
#if UNBOUND
  let wtd = wd - wfd
#else
  let wtd = fromIntegral $ wd - wfd :: Double
#endif
  let aud = wtd / fromIntegral nUpdates
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

