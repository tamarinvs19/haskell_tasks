import qualified Data.Sequence as S

data Tree a = Nil | Branch (Tree a) a (Tree a)  deriving (Eq, Show)

newtype Preorder a = PreO (Tree a) deriving (Eq, Show)
newtype Postorder a = PostO (Tree a) deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap f Nil = Nil
  fmap f (Branch l a r) = (Branch (fmap f l) (f a) (fmap f r))

instance Foldable Tree where
  foldr _ ini Nil = ini
  foldr f ini (Branch l a r) = foldr f (f a (foldr f ini r)) l

instance Foldable Postorder where
  foldr _ ini (PostO (Nil)) = ini
  foldr f ini (PostO (Branch l a r)) = foldr f (foldr f (f a ini) (PostO r)) (PostO l)

instance Foldable Preorder where
  foldr _ ini (PreO (Nil)) = ini
  foldr f ini (PreO (Branch l a r)) = f a (foldr f (foldr f ini (PreO r)) (PreO l)) 

bfs :: (a -> b -> b) -> b -> S.Seq (Levelorder a) -> b
bfs f ini S.Empty = ini 
bfs f ini ((LevelO (Nil)) S.:<| t) = bfs f ini t 
bfs f ini ((LevelO (Branch l a r)) S.:<| t) = 
  f a (bfs f ini (t S.:|> (LevelO l) S.:|> (LevelO r))) 

instance Foldable Levelorder where
  foldr f ini tree = bfs f ini (S.fromList [tree])

