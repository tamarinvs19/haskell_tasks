absDiff :: Num a => [a] -> [a]
absDiff [] = []
absDiff xs = do 
  (a, b) <- tail (zip xs (0:xs))
  pure (abs $ b - a)

