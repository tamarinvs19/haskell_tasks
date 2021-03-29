digits :: Integer -> [Integer]
digits = digits' . abs
digits' n 
  | n < 10 = [n]
  | otherwise = (digits' (n `div` 10)) ++ [n `mod` 10]

containsAllDigits :: Integer -> Bool
containsAllDigits n = containsAllDigitsInList [1..9] (digits n)

containsAllDigitsInList :: [Integer] -> [Integer] -> Bool
containsAllDigitsInList [] _ = True
containsAllDigitsInList (i:is) ds = (containsDigit i ds) && (containsAllDigitsInList is ds)

containsDigit :: Integer -> [Integer] -> Bool
containsDigit i [] = False
containsDigit i (d:ds) = (i == d) || (containsDigit i ds)
