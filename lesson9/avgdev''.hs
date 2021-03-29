import System.Random
import GHC.Float

rand = randomRs (0, 1) (mkStdGen 239)

avg :: Int -> [Int] -> Double
avg n ls = abs $ (int2Double $ sum ls) - 0.5 * (int2Double n)

getRandPart :: Int -> Int -> [Int] -> [Double]
getRandPart 0 _ _ = []
getRandPart k n rs = 
  (avg n (fst spl)) : (getRandPart (k-1) n (snd spl)) where
    spl = splitAt n rs

avgdev'' :: Int -> Int -> Double
avgdev'' k n = (sum (getRandPart k n rand)) / (int2Double k)



