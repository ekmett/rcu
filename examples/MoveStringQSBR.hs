{-# LANGUAGE FlexibleContexts #-}
import Control.Concurrent.MVar (takeMVar)
import Control.Concurrent.RCU.QSBR
import Control.Concurrent.RCU.Class
import Control.Monad (forM, forM_, replicateM)
import Data.List (group, intercalate)
import Debug.Trace (trace)
import Prelude hiding (read)

data List s a = Nil | Cons a (SRef s (List s a))

snapshot :: MonadReading (SRef s) (m s) => [a] -> List s a -> m s [a]
snapshot acc Nil         = return $ reverse acc
snapshot acc (Cons x rn) = snapshot (x : acc) =<< readSRef rn

reader :: Int -> [[a]] -> SRef s (List s a) -> RCU s [[a]]
reader 0 acc _    = return $ reverse acc
reader n acc head = do
  l <- reading $ snapshot [] =<< readSRef head
  reader (n - 1) (l : acc) head

deleteMiddle :: SRef s (List s a) -> WritingRCU s ()
deleteMiddle rl = do
  Cons a rn <- readSRef rl
  Cons _ rm <- readSRef rn
  writeSRef rl $ Cons a rm 

moveDback :: SRef s (List s Char) -> WritingRCU s ()
moveDback rl = do
  Cons a rb <- readSRef rl
  Cons b rc <- readSRef rb
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

testList :: RCU s (SRef s (List s Char))
testList = do
  tail <- newSRef Nil
  c1   <- newSRef $ Cons 'E' tail
  c2   <- newSRef $ Cons 'D' c1
  c3   <- newSRef $ Cons 'C' c2
  c4   <- newSRef $ Cons 'B' c3
  newSRef $ Cons 'A' c4

compactShow :: (Show a, Eq a) => [a] -> String
compactShow xs = intercalate ", " $ map (\xs -> show (length xs) ++ " x " ++ show (head xs)) $ group xs

main :: IO ()
main = do 
  outs <- runRCU $ do
    -- initialize list
    head <- testList
    -- spawn 8 readers, each records 100000 snapshots of the list
    rts <- replicateM 8 $ forking $ reader 100000 [] head
    -- spawn a writer to move a node from a later position to an earlier position
    wt  <- forking $ writing $ moveDback head
    
    -- wait for the readers to finish and print snapshots
    outs <- forM rts $ \rt -> do 
      v <- joining rt
      return $ show (rcuThreadId rt) ++ ": " ++ compactShow v
    -- wait for the writer to finish
    joining wt
    return outs
  forM_ outs putStrLn
