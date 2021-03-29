data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC x y z = listToOddC $ ((oddCToList x) ++ (oddCToList y) ++ (oddCToList z))

oddCToList :: OddC a -> [a]
oddCToList (Un x) = [x]
oddCToList (Bi x y c) = x:y:(oddCToList c)

listToOddC :: [a] -> OddC a
listToOddC (x:y:ls) = Bi x y (listToOddC ls)
listToOddC (x:ls) = Un x
listToOddC [x] = Un x 
