doubleFact :: Integer -> Integer
doubleFact n = if n >= 2 then n * doubleFact (n-2) else 1

