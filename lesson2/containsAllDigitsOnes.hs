digits :: Integer -> [Integer]
digits = digits' . abs
digits' n 
  | n < 10 = [n]
  | otherwise = (digits' (n `div` 10)) ++ [n `mod` 10]

containsAllDigitsOnes :: Integer -> Bool
containsAllDigitsOnes n = containsAllDigitsInList [1..9] (digits n)

containsAllDigitsInList :: [Integer] -> [Integer] -> Bool
containsAllDigitsInList [] _ = True
containsAllDigitsInList (i:is) ds = (containsDigitOnes i ds) && (containsAllDigitsInList is ds)

containsDigitOnes :: Integer -> [Integer] -> Bool
containsDigitOnes i [] = False
containsDigitOnes i (d:ds) = (i == d) /= (containsDigitOnes i ds)

