sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = if x == 0
                then (0, 1)
                else helper (0, 0) (abs x)
helper (s, c) x = if x == 0
                  then (s, c)
                  else helper (s + (x `mod` 10), c + 1) (x `div` 10)

