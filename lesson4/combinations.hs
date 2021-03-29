comb :: Int -> [a] -> [[a]]
comb n xs = comb' n xs [] []

comb' 0 _ prefix otherRes = reverse prefix : otherRes
comb' _ [] prefix otherRes = otherRes
comb' n (x:xs) prefix otherRes = 
	comb' (n-1) xs (x:prefix) (comb' n xs prefix otherRes)

