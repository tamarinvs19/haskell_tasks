import Control.Monad.Writer
minusLoggedL :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedL a ls = minusLoggedL' (writer (a, show a)) ls

first (a, w) = a
second (a, w) = w

template :: (Show a, Num a) => a -> String -> String
template n s = "(" ++ s ++ "-" ++ (show n) ++ ")" 

minusLoggedL' :: (Show a, Num a) => Writer String a -> [a] -> Writer String a
minusLoggedL' x [] = x
minusLoggedL' x (l:ls) = let x' = writer ((first (runWriter x)) - l, template l (second (runWriter x)))
                         in minusLoggedL' x' ls
