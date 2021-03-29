rotate :: Int -> [a] -> [a]
rotate n xs | n >= 0 = rotatePositive n xs
						| otherwise = rotatePositive ((length xs) + (n `mod` (length xs))) xs

rotatePositive :: Int -> [a] -> [a]
rotatePositive n xs = map snd (zip xs (drop n (cycle xs)))
