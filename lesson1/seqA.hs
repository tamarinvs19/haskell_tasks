seqA :: Int -> Integer
seqA = (map seqA' [0 ..] !!) 
    where seqA' 0 = 1; seqA' 1 = 2; seqA' 2 = 3; seqA' n = seqA (n - 1) + seqA (n - 2) - (2 * seqA (n-3))






