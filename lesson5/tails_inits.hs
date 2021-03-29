tails' :: [a] -> [[a]]
tails' = foldr fun ini
fun l [] = [[l]]
fun l acc = (l : (acc !! 0)) : acc
ini = [[]]

inits' :: [a] -> [[a]]
inits' = foldr fun' ini'
fun' l acc = [] : [l : xs | xs <- acc]
ini' = [[]]
