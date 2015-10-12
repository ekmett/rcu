{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
import Control.Concurrent.RCU.MODE
import Control.Concurrent.RCU.Class
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (replicateM, replicateM_)
import Data.Bits ((.&.), (.|.), shiftL)
import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Word (Word64)
--import Debug.Trace (trace)
import Options.Applicative (auto, execParser, fullDesc, help, info, long, metavar, option, progDesc, short)
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

data Opts = Opts { nReaders :: Int
                 , nUpdates :: Int }

main :: IO ()
main = do 
  Opts { nReaders, nUpdates } <- execParser opts
  putStrLn $ "readers: " ++ show nReaders ++ ", updates: " ++ show nUpdates
  let nTotal = fromIntegral $ nUpdates * nReaders :: Double
  (ods, wd, wfd) <- runRCU $ do
    -- initialize list
    hd  <- testList
    -- initialize flag writer uses to stop readers
    rf  <- newSRef False
    -- spawn nReaders readers, each takes snapshots of the list until the writer has finished
    rts <- replicateM nReaders 
         $ forking $ reading $ ReadingRCU 
         $ \ s -> do beg <- getCurrentTime
                     x   <- evaluate . force =<< unRCU (reader rf 0 hd) s -- how long do the snapshots take?
                     mid <- getCurrentTime
                     _   <- evaluate . force $ x -- how long does walking over the result take?
                     end <- getCurrentTime
                     return ( x
                            , mid `diffUTCTime` beg
                            , end `diffUTCTime` mid )
    -- spawn a writer to move a node from a later position to an earlier position nUpdates times
    wt  <- forking $ writing $ WritingRCU 
         $ \ s -> do beg <- getCurrentTime
                     x   <- evaluate . force =<< runWritingRCU (replicateM_ nUpdates $ moveDback hd) s
                     runWritingRCU (writeSRef rf True) s
                     mid <- getCurrentTime
                     _   <- evaluate . force $ x
                     end <- getCurrentTime
                     return ( mid `diffUTCTime` beg
                            , end `diffUTCTime` mid )
    -- wait for the readers to finish
    ods <- mapM joining rts
    -- wait for the writer to finish
    (wd, wfd) <- joining wt
    return (ods, wd, wfd)
  let outs = map a ods
      rds  = map b ods
      rfds = map c ods
      a (x,_,_) = x
      b (_,y,_) = y
      c (_,_,z) = z
  putStrLn $ "dups by thread:" ++ (intercalate ", " $ zipWith (\ i dups -> show i ++ ": " ++ show dups) [(1 :: Integer)..] outs)
  putStrLn $ "average dups per update: " ++ show (fromIntegral (sum outs) / nTotal)
  putStrLn $ "reader times: " ++ show rds
  putStrLn $ "reader evaluate . force times: " ++ show rfds
  putStrLn $ "writer time: " ++ show wd
  putStrLn $ "writer evaluate . force time: " ++ show wfd
  putStrLn $ "average writer update time: " ++ show ((wd - wfd) / fromIntegral nUpdates)
  where opts = info optsParser
             ( fullDesc
            <> progDesc "Measure writer latency for synchronize." )
        optsParser = Opts <$> nReadersParser <*> nUpdatesParser
        nReadersParser = option auto
                       ( long "readers"
                      <> short 'r'
                      <> metavar "N"
                      <> help "Spawn N reader threads" )
        nUpdatesParser = option auto
                       ( long "updates"
                      <> short 'u'
                      <> metavar "M"
                      <> help "Writer thread performs M updates" )
        
