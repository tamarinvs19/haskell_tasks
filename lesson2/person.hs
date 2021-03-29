data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

squeezeName :: String -> String
squeezeName name 
  | length name > 2 = [name !! 0] ++ ['.'] 
  | otherwise = name

abbrFirstName :: Person -> Person
abbrFirstName p = Person { firstName = squeezeName $ firstName p, lastName = lastName p, age = age p }

