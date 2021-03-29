digits :: Integer -> [Integer]
digits = digits' . abs
digits' n 
  | n < 10 = [n]
  | otherwise = (digits' (n `div` 10)) ++ [n `mod` 10]
