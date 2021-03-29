import Control.Monad.Writer
minusLoggedR :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedR x [] = writer (x, show x) 
minusLoggedR x (l:ls) = writer (l - old_value, template l old_string) 
  where
    old = minusLoggedR x ls
    old_value = first $ runWriter old
    old_string = second $ runWriter old

first (a, w) = a
second (a, w) = w

template :: (Show a, Num a) => a -> String -> String
template n s = "(" ++ (show n) ++ "-" ++ s ++ ")" 
