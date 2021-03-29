{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
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

data B a = Empty | One a | Zero a deriving (Eq,Show)

instance Functor B where
  fmap f (One a) = (One (f a))
  fmap f (Zero a) = (Zero (f a))
  fmap _ Empty = Empty

type Bin = Fix B

phiB :: B Int -> Int
phiB Empty = 0
phiB (One a) = 1 + 2 * a
phiB (Zero a) = 0 + 2 * a

bin2int :: Bin -> Int
bin2int = cata phiB

psiB :: Int -> B Int
psiB 0 = Empty
psiB n = if n `mod` 2 == 0 then Zero (n `div` 2) else One ((n-1) `div` 2)

int2bin :: Int -> Bin
int2bin = ana psiB
