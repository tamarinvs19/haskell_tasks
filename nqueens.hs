import Control.Monad

queens n = foldM (\y _ -> [ x : y | x <- [1..n], safe x y 1]) [] [1..n]
safe x [] n = True
safe x (c:y) n = and [ x /= c, x /= c + n, x /= c - n, safe x y (n+1)]

main = mapM_ print $ queens 8
