reverse' :: [a] -> [a]
reverse' = foldr fun' ini'
fun' l acc = acc ++ [l]
ini' = []

reverse'' :: [a] -> [a]
reverse'' = foldl fun'' ini''
fun'' acc l = l : acc
ini'' = []
