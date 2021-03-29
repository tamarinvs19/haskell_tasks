infixl 9 !!!

(!!!) :: [a] -> Int -> Maybe a
xs !!! n = foldr fun ini xs n
fun :: a -> (Int -> Maybe a) -> Int -> Maybe a
fun x g index
	| index < 0 = Nothing
 | index == 0 = Just x
 | otherwise = g $ index - 1

ini :: Int -> Maybe a
ini _ = Nothing
