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

