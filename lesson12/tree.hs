{-# LANGUAGE InstanceSigs, DeriveFunctor, StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
newtype Fix f = In (f (Fix f))

deriving instance Show (f (Fix f)) => Show (Fix f)
deriving instance Eq (f (Fix f)) => Eq (Fix f)

out :: Fix f -> f (Fix f)
out (In x) = x

type Algebra f a = f a -> a

cata :: Functor f => Algebra f a -> Fix f -> a
cata phi (In x) = phi $ fmap (cata phi) x

type Coalgebra f a = a -> f a 

ana :: Functor f => Coalgebra f a -> a -> Fix f
ana psi x = In $ fmap (ana psi) (psi x)

hylo :: Functor f => Algebra f a -> Coalgebra f b -> (b -> a)
hylo phi psi = cata phi . ana psi


data T a x = Leaf | Branch x a x deriving (Show)

instance Functor (T a) where
  fmap f (Branch l x r) = (Branch (f l) x (f r))
  fmap _ Leaf = Leaf

type Tree a = Fix (T a) 

phiTSum :: Algebra (T Integer) Integer -- T Integer -> Integer
phiTSum Leaf = 0
phiTSum (Branch l x r) = l + x + r

treeSum :: Tree Integer -> Integer
treeSum = cata phiTSum

iB l x r = In $ Branch l x r
iL = In Leaf
testTree = iB (iB (iB iL 2 iL) 3 (iB iL 4 iL)) 5 (iB iL 6 (iB iL 7 iL))


phiTInorder :: Algebra (T a) [a] -- T a [a] -> [a]
phiTInorder Leaf = []
phiTInorder (Branch l x r) = l ++ [x] ++ r

tree2listInorder :: Tree a -> [a]
tree2listInorder = cata phiTInorder

psiTBST :: Ord a => Coalgebra (T a) [a]    -- [a] -> T a [a]
psiTBST (x:xs) = Branch (filter (< x) xs) x (filter (>= x) xs)
psiTBST [] = Leaf

list2BST :: Ord a => [a] -> Tree a
list2BST = ana psiTBST

sort :: Ord a => [a] -> [a]
sort = hylo phiTInorder psiTBST

