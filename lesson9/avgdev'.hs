import System.Random
import Control.Monad.State
import GHC.Float

randomRState :: (Random a, RandomGen g) => (a, a) -> State g a
randomRState (lo, hi) = state $ (\g -> (randomR (lo, hi) g)) 

avgdev' :: Int -> Int -> State StdGen Double
avgdev' k n = do
  val <- replicateM k $ test n 
  return ((sum val) / (int2Double k))

test :: Int -> State StdGen Double
test n = do
  xs <- replicateM n $ randomRState (0, 1)
  return (abs $ (int2Double $ sum xs)  - 0.5 * (int2Double n))

sumLists :: (Num a) => IO [a] -> IO a 
sumLists xs = do
  x <- xs
  return (sum x)
