repeatEveryElem :: Int -> [a] -> [a]
repeatEveryElem count [] = []
repeatEveryElem count (first:list) = (repeatElem count first) ++ (repeatEveryElem count list)

repeatElem :: Int -> a -> [a]
repeatElem 0 _ = []
repeatElem count a = [a] ++ (repeatElem (count - 1) a)
