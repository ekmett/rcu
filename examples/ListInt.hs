import Control.Concurrent.MVar (takeMVar)
import Control.Concurrent.RCU
import Control.Monad (forM, forM_, replicateM)
import Data.List (group, intercalate)
import Prelude hiding (read)

data List s a = Nil | Cons a (SRef s (List s a))

snapshot :: Int -> List s Int -> ReadingRCU s Int
snapshot acc Nil         = return acc
snapshot acc (Cons x rn) = snapshot (x + acc) =<< readSRef rn

reader :: Int -> Int -> SRef s (List s Int) -> ReadingRCU s Int
reader 0 acc _    = return acc
reader n acc head = do
  acc' <- snapshot acc =<< readSRef head
  reader (n - 1) acc' head

deleteMiddle :: SRef s (List s a) -> WritingRCU s ()
deleteMiddle rl = do
  Cons a rn <- readSRef rl
  Cons _ rm <- readSRef rn
  writeSRef rl $ Cons a rm 

testList :: RCU s (SRef s (List s Int))
testList = do
  tail <- newSRef Nil
  c1   <- newSRef $ Cons (- 1) tail
  c2   <- newSRef $ Cons 1     c1
  newSRef $ Cons 1 c2

main :: IO ()
main = do 
  outs <- runRCU $ do
    -- initialize list
    head <- testList
    -- spawn 8 readers, each records 100000 snapshots of the list
    rts <- replicateM 8 $ forking $ reading $ reader 100000 0 head
    -- spawn a writer to delete the middle node
    wt  <- forking $ writing $ deleteMiddle head
    
    -- wait for the readers to finish and print snapshots
    outs <- forM rts $ \rt -> do 
      v <- joining rt
      return $ show (rcuThreadId rt) ++ ": " ++ show v
    -- wait for the writer to finish
    joining wt
    return outs
  forM_ outs putStrLn
