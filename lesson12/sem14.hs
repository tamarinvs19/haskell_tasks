{-# LANGUAGE InstanceSigs, StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}

module Sem14 where

import Data.List(genericReplicate, genericLength)

-- Рекурсивные типы. Помните, у нас был комбинатор неподвижной точки для лямбда-термов
-- Для каждого данного ему терма он возвращал его неподвижную точку.
-- Сегодня предлагается сделать то же самое, но подняться на уровень типов
-- Для размники предлагается доказать, что следующие типы изоморфны:

-- 1. (Integer, Integer) и (Bool -> Integer)
-- 2. Either Bool Bool и (Bool, Bool)
-- 3. Integer и [()]
-- 4. Tree a и Tree' a

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show)

data Tree' a = Empty' | Node' a (Bool -> Tree' a)

instance Show a => Show (Tree' a) where
    showsPrec _ Empty' = showString "Empty'"
    showsPrec d (Node' x f) = showParen (d > app_prec) $
        showString "Node' " .
        showsPrec (app_prec + 1) x .
        showChar ' ' .
        showsPrec (app_prec + 1) (f True) .
        showChar ' ' .
        showsPrec (app_prec + 1) (f False)
            where app_prec = 10

instance Eq a => Eq (Tree' a) where
    Empty'      == Empty'       = True
    Empty'      == x            = False
    x           == Empty'       = False
    (Node' x f) == (Node' y g)  = x == y && f True == g True && f False == g False

-- Для доказательства достаточно написать соответствующие взаимно-обратные функции

fromP :: (Integer, Integer) -> (Bool -> Integer)
fromP (a, b) = f where
  f True = a
  f False = b

toP :: (Bool -> Integer) -> (Integer, Integer)
toP f = (f True, f False) 


fromE :: Either Bool Bool -> (Bool, Bool)
fromE (Left a) = (True, a)
fromE (Right b) = (False, b)

toE :: (Bool, Bool) -> Either Bool Bool
toE (True, a) = Left a
toE (False, a) = Right a


fromI :: Integer -> [()]
fromI 0 = []
fromI n = ():(fromI (n-1))

toI :: [()] -> Integer
toI [] = 0
toI (l:ls) = 1 + toI $ ls


fromT :: Tree a -> Tree' a
fromT = undefined

toT :: Tree' a -> Tree a
toT = undefined

newtype Fix f = Fix (f (Fix f))

deriving instance Show (f (Fix f)) => Show (Fix f)
deriving instance Eq (f (Fix f)) => Eq (Fix f)

unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x

-- Сконструируем, например, нерекурсивный тип для списка

data L a l = Nil | Cons a l deriving (Eq, Show)

-- Инстанс функтора для него

instance Functor (L a) where
    fmap :: (u -> v) -> L a u -> L a v
    fmap _ Nil = Nil
    fmap g (Cons x y) = Cons x (g y)

-- Примеры списков, имеющих тип нашего нерекурсивного функтора
-- Nil :: L a l -- пустой список
-- Cons 1 0 :: L Integer Integer
-- Cons 1 (Cons 2 Nil) :: L Integer (L Integer (L a l))

-- Рекурсивный тип для списка -- это просто неподвижная точка L

type List a = Fix (L a)

-- Примеры списков, сконструированных таким образом

-- Fix Nil -- пустой список
-- Fix (Cons '1' (Fix Nil))
-- Fix (Cons '2' (Fix (Cons '1' (Fix Nil))))

-- Покажем, что наше определение списка полностью изоморфно обычному списку из хаскеля

from :: [a] -> List a
from = undefined

to :: List a -> [a]
to = undefined

-- Катаморфизмы и анаморфизмы.
-- В теории категорий есть понятие F-алгебры, которое позволяет обобщить понятие алгебраической структуры
-- Это позволяет записывать законы в виде морфизмов, не думая вообще о том, что лежит внутри этой структуры
-- Хороший пример про группы написан в английской википедии - [0], например.
-- Кроме того, есть просто отличная статья от Бартоша Милевски - [1], где он раскладывает все по полочкам.
-- Если сложно читать на английском, то есть русский перевод - [3].
-- Если хочется упороться совсем, есть статья [4], в которой впервые и описаны эти концепции

-- F-алгебры образуют категорию, объекты которой -- это сами алгебры над некоторым эндофунктором F
-- а морфизмы -- так называемые гомоморфизмы F-алгебр -- это стрелки, которые сохраняют структуру
-- Нам достаточно думать о F-алгебре как о функторе f, некотором объекте-носителе(carrier) а
-- и морфизме phi :: f a -> a. Удобно думать, что функтор F -- формирует выражения в этой структуре, а
-- морфизм phi -- вычисляет их

type Algebra f a = f a -> a

-- Катаморфизм представляет собой некоторое обобщение понятия свертки.
-- Продолжая наш поход в теоретико-категориальных лесах, катаморфизм -- это уникальный
-- гомоморфизм из начальной(initial) алгебры в некоторую другую алгебру. Так и запишем:

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg (Fix x) = alg $ fmap (cata alg) x

--  Для примера рассмотрим определение нерекурсивного функтора, который кодирует натуральные числа

data N x = Z | S x deriving Show

instance Functor N where
    fmap f Z = Z
    fmap f (S x) = S $ f x

type Nat = Fix N

phiN :: Algebra N Int -- N Int -> Int
phiN Z = 0
phiN (S x) = x + 1

toInt :: Nat -> Int
toInt = cata phiN

-- Дуальной(двойственной) конструкцией для F-алгебры является F-коалгебра

type Coalgebra f a = a -> f a

-- Они точно так же образуют категорию, с теми же объектами, и теми же гомоморфизмами в роли стрелок
-- О них можно думать, как о способе породить некоторую(возможно бесконечную) структуру.

-- Дуальным к катаморфизму является анаморфизм -- обобщение "разветки"
-- Это уникальный гомоморфизм из произвольной коалгебры в терминальную(terminal)
-- Определим его:

ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coalg x = Fix $ fmap (ana coalg) (coalg x)

-- Например:

psiN :: Coalgebra N Int -- Int -> N Int
psiN 0 = Z
psiN n = S (n - 1)

toNat :: Int -> Nat
toNat = ana psiN

-- Наконец, есть гилеморфизм  -- последовательное применение сначала анаморфизма, а затем катаморфизма

hylo :: Functor f => Algebra f a -> Coalgebra f b -> b -> a
hylo alg coalg = cata alg . ana coalg

-- Есть еще метаморфизм, когда мы сначала лепим катаморфизм, а затем анаморфизм, но я не могу привести
-- ни одного содержательного примера =(

meta :: (Functor f, Functor g) => Coalgebra f a -> Algebra g a -> Fix g -> Fix f
meta coalg alg = ana coalg . cata alg

-- На лекции вам показали некоторое количество списочных алгебр.
-- Давайте посмотрим на коалгебру для списка, которая будет вычислять нам простые числа

-- Нам понадобится нерекурсивный функтор, который описывает бесконечный список

data St e s = St e s deriving Show

instance Functor (St e) where
    fmap f (St e x) = St e (f x)

type Stream e = Fix (St e)

-- Предлагается понять, как устроена коалгебра для потока простых чисел
-- Какой тип надо ей написать?

data Undefined

primesCoalg :: Undefined
primesCoalg = undefined

-- Ну и наконец, поток, это тот же бесконечный список поэтому предлагается написать для него алгебру
-- которая преобразует в обычный список из хаскеля

toInfListAlg :: Algebra (St e) [e] -- St e [e] -> [e]
toInfListAlg = undefined 

toInfList :: Stream a -> [a]
toInfList = cata toInfListAlg


-- firstNPrimes n = take n $ toInfList $ ana primesCoalg [2..]

-- Ссылки:
-- 0. https://en.wikipedia.org/wiki/F-algebra#Groups
-- 1. https://bartoszmilewski.com/2017/02/28/f-algebras/
-- 2. https://henrychern.files.wordpress.com/2017/10/24.pdf
-- 3. https://ris.utwente.nl/ws/portalfiles/portal/6142049
