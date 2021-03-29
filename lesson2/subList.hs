sublist :: Int -> Int -> [a] -> [a]
sublist left right list = drop left . take right $ list

