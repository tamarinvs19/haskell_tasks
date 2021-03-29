{-# LANGUAGE InstanceSigs #-}
data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  -- fmap :: (a -> b) -> (Tree a) -> (Tree b)
  fmap _ Nil = Nil
  fmap f (Branch l a r) = Branch (fmap f l) (f a) (fmap f r)

instance Applicative Tree where
  -- pure :: a -> (Tree a)
  pure a = Branch (pure a) a (pure a)
  -- (<*>) :: (Tree (a -> b)) -> (Tree a) -> (Tree b)
  (Branch l f r) <*> (Branch l' a r') = Branch (l <*> l') (f a) (r <*> r')
  _ <*> _ = Nil

instance Foldable Tree where
  foldr _ ini Nil = ini
  foldr f ini (Branch l a r) = foldr f (f a (foldr f ini r)) l

instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Nil = pure Nil
  traverse g (Branch l a r) = Branch <$> (traverse g l) <*> (g a) <*> (traverse g r)
