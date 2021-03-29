factor2 :: Integer -> [(Integer, Integer)]
factor2 n = 
  filter (\(a, b) -> and [(a*b == n), (a <= b)]) (factor2' n)

factor2' :: Integer -> [(Integer, Integer)]
factor2' n = do
  x <- [1..(round (sqrt (fromInteger n)))]
  pure (x, n `div` x)
  
