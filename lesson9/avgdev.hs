import Control.Monad
import System.Random
import GHC.Float

avgdev :: Int -> Int -> IO Double
avgdev k n = genTests k n

genTests k n = do 
  s <- sumLists $ replicateM k (do {t <- test n; return t})
  return (s / (int2Double k))
  

test :: Int -> IO Double
test n = do
  s <- sumLists $ (replicateM n generator)
  return (abs $ (int2Double s) - (0.5 * (int2Double n)))

generator :: IO Int
generator = do
  val <- ((<$>) (\x -> mod x 2) (getStdRandom next))
  return val

sumLists :: (Num a) => IO [a] -> IO a 
sumLists xs = do
  x <- xs
  return (sum x)

