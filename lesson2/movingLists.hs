movingLists :: Int -> [a] -> [[a]]
movingLists n [] = [] 
movingLists n list 
  | checkCount n list = [getElements n list] ++ (movingLists n (moveByOne list))
  | otherwise = []

moveByOne :: [a] -> [a]
moveByOne [] = []
moveByOne (first:list) = list

checkCount :: Int -> [a] -> Bool
checkCount 0 _ = True
checkCount _ [] = False
checkCount n (x:xs) = checkCount (n-1) xs

getElements :: Int -> [a] -> [a]
getElements 0 _ = []
getElements _ [] = []
getElements n (first:list) = [first] ++ (getElements (n-1) list)
