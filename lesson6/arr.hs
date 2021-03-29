newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

instance Functor (Arr2 e1 e2) where
  -- fmap f (Arr2 g) = Arr2 ((fmap . fmap) f g) 
  fmap f (Arr2 a) = Arr2 ((f .) . a)

instance Functor (Arr3 e1 e2 e3) where
  fmap f (Arr3 a) = Arr3 (((f .) .) . a)

instance Applicative (Arr2 e1 e2) where
  pure f = (Arr2 (\ee1 -> \ee2 -> f ))
  (Arr2 f) <*> (Arr2 g) = (Arr2 (\ee1 -> \ee2 -> (f ee1 ee2 (g ee1 ee2))))

instance Applicative (Arr3 e1 e2 e3) where
  pure f = (Arr3 (\ee1 -> \ee2 -> \ee3 -> f ))
  (Arr3 f) <*> (Arr3 g) = (Arr3 (\ee1 -> \ee2 -> \ee3 -> (f ee1 ee2 ee3 (g ee1 ee2 ee3))))
