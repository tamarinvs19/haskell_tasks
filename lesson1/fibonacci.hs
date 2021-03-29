fibonacci :: Integer -> Integer

fibonacci n = if n >= 0 
              then helper 0 1 n 
              else (sgn n) * (helper 0 1 (-n))
    where helper last1 last2 n = if n == 0 
                                 then last1
                                 else helper last2 (last1 + last2) (n-1)

sgn n | n `mod` 2 == 1 = 1
      | otherwise = -1
