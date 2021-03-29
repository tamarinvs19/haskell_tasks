data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

instance Functor OddC where
  fmap f (Un x) = Un (f x)
  fmap f (Bi x y z) = (Bi (f x) (f y) (fmap f z))

instance Applicative OddC where
  pure x = (Un x)
  (Un f) <*> (Un x) = Un (f x)
  (Bi f g h) <*> (Un x) = Bi (f x) (g x) (h <*> Un x)
  (Un f) <*> (Bi x y z) = Bi (f x) (f y) (Un f <*> z)
  (Bi f g h) <*> o = concat3OC (Un f <*> o) (Un g <*> o) (h <*> o) 

instance Monad OddC where
  return a = Un a
  xs >>= k = concatOC (fmap k xs)

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un x) = listToOddC $ oddCToList x 
concatOC (Bi x y z) = concat3OC x y (concatOC z)

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC x y z = listToOddC $ ((oddCToList x) ++ (oddCToList y) ++ (oddCToList z))

oddCToList :: OddC a -> [a]
oddCToList (Un x) = [x]
oddCToList (Bi x y c) = x:y:(oddCToList c)

listToOddC :: [a] -> OddC a
listToOddC (x:y:ls) = Bi x y (listToOddC ls)
listToOddC (x:ls) = Un x
listToOddC [x] = Un x 

t1 = Bi 10 20 (Un 30)
t2 = Bi 1 2 (Bi 3 4 (Un 5))  
f1 = Bi (+100) (+200) (Un (*10))
f2 = Bi (+1000) (+2000) (Bi (*100) (*1000) (Un (+1)))
