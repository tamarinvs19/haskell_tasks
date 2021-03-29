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


data E e = Num Int | Add e e | Mult e e 

type Expr = Fix E

instance Functor E where
  fmap f (Add x y) = Add (f x) (f y)
  fmap f (Mult x y) = Mult (f x) (f y) 
  fmap f (Num i) = Num i

phiE :: E Int -> Int
phiE (Num x) = x
phiE (Add x y) = x + y
phiE (Mult x y) = x * y

eval :: Expr -> Int
eval = cata phiE

delFL s = tail $ take (length s - 1) s

phiEShow :: E String -> String
phiEShow (Num x) = show x
phiEShow (Add x y) = "(" ++ (delFL (show x)) ++ "+" ++ (delFL $ show y) ++ ")"
phiEShow (Mult x y) = "(" ++ (delFL $ show x) ++ "*" ++ (delFL $ show y) ++ ")"

phiEShowS :: E ShowS -> ShowS
phiEShowS (Num x) s    = s ++ (show x)
phiEShowS (Add x y) s  = s ++ (x "+ ") ++ (y " ")
phiEShowS (Mult x y) s = s ++ (x "* ") ++ (y " ")


type Stack = [Int]

push :: Int -> Stack -> Stack
push a as = a : as

add :: Stack -> Stack
add  (a : b : cs) = (b + a) : cs

mult :: Stack -> Stack
mult (a : b : cs) = (b * a) : cs

phiE' :: E (Stack -> Stack) -> Stack -> Stack
phiE' (Num x) stack = push x stack
phiE' (Add x y) stack = add 

eval' :: Expr -> Stack -> Stack
eval' = cata phiE'

en = In . Num
e3     = en 3
ep35   = In (Add e3 (en 5))
emp357 = In (Mult ep35 (en 7))
em7p35 = In (Mult (en 7) ep35)
