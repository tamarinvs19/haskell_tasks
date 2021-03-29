{-# LANGUAGE InstanceSigs #-}
data Triple a = Tr a a a deriving (Eq,Show)

instance Functor Triple where
--  fmap :: (a -> b) -> (Triple a) -> (Triple b)
  fmap f (Tr x y z) = Tr (f x) (f y) (f z)

instance Applicative Triple where
--  pure :: a -> (Triple a)
  pure x = (Tr x x x)
  (Tr f g h) <*> (Tr x y z) = (Tr (f x) (g y) (h z))

instance Foldable Triple where
	foldr f init (Tr x y z) = foldr f init [x, y, z]

instance Traversable Triple where
	traverse f (Tr x y z) = Tr <$> (f x) <*> (f y) <*> (f z)
