{-# LANGUAGE InstanceSigs #-}
module Sem11 where

import Control.Monad(ap, replicateM, replicateM_, filterM, liftM, forM, forM_)
import qualified Data.Set as Set
import System.Random
import Control.Arrow
import Data.IORef

-- Стандартные монады. Reader, Writer, State и IO
-- 
-- УВАЖАЕМЫЕ ЗНАТОКИ!
-- КАК ВЫ ДУМАЕТЕ, ЧТО ДЕЛАЕТ ЭТОТ КОД:
-- join :: Monad m => m (m a) -> m a
-- f = join (*)

-- Для разминки интересно бывает посмотреть на то, насколько Хаскель
-- иногда все же ленив. Попробуйте _угадать_, что выведется на экран:
-- let x = print "A" in print "B"
-- let x = print "A" in x >> print "B"
-- (\x -> print "A") (print "B")
-- (print "A") `seq` (print "B")

-- Ну а теперь про монаду Reader. На лекции у вас было вот такое определение:

newtype Reader r a = Reader { runReader :: r -> a }

instance Monad (Reader r) where
    return      = Reader . const
    m >>= k     = Reader $ \e ->    let v = runReader m e
                                    in runReader (k v) e

-- На лекции вам показывали интерфейс сборки-разборки Reader в виде двух функций:
-- reader :: (r -> a) -> Reader r a
-- runReader :: Reader r a -> (r -> a)

-- Тут вообще просят написать reader, но очевидно, что для нашего типа это просто констурктор Reader

-- Напишем инстансы Applicative и Functor для него

instance Functor (Reader r) where
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap = liftM 

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure = return
    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (<*>) = ap

-- Наконец, обеспечьте нашем определению монады Reader стандартный интерфейс
-- do 
--  ..
--  ..
--  e <- ask
--  ...
ask :: Reader r r
ask = Reader id

-- do
--  ..
--  ..
--  e' <- asks show
asks :: (r -> a) -> Reader r a
asks f = Reader f

local :: (r -> r) -> Reader r a -> Reader r a
local f (Reader g) = Reader $ g . f

-- Штука выше вообще говоря обобщается.
-- Кроме того, что бы поменять локально значение в окружении, можно поменять его тип

local' :: (r -> r') -> Reader r' a -> Reader r a
local' f (Reader g) = Reader $ g . f


exampleReader :: Reader Int [Int]
exampleReader = do
  e <- ask
  let x = e * 2
  e' <- asks succ
  let y = e'
  pure [x, y]


yetAnotherExampleReader :: Reader String Int
yetAnotherExampleReader = do
  e <- ask
  let l = length e
  newEnv <- local (++ "fubar") ask
  e' <- ask
  let l' = length newEnv
  pure l'


-- Монада Writer
-- Я уже показывал простейший пример того, как можно логгировать вычисления, используя, например, пару
-- Кто бы мог подумать, что мы будем работать с очень похожим определением

newtype Writer w a = Writer { runWriter :: (a, w) }

instance Functor (Writer w) where
    fmap :: (a -> b) -> Writer w a -> Writer w b
    fmap f (Writer (x, w)) = Writer (f x, w)

instance (Monoid w) => Applicative (Writer w) where
    pure :: a -> Writer w a
    pure x = Writer (x, mempty)
    (<*>) :: Writer w (a -> b) -> Writer w a -> Writer w b
    (Writer (f, u)) <*> (Writer (x, v)) = Writer (f x, u <> v)

instance (Monoid w) => Monad (Writer w) where
    return :: a -> Writer w a
    return x    = Writer (x, mempty)
    (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
    m >>= k     = let   (x, l1) = runWriter m
                        (y, l2) = runWriter $ k x
                    in Writer (y, l1 <> l2)

-- Вам предлагается снова наделить наше определение стандартным интерфейсом

tell :: Monoid w => w -> Writer w ()
tell x = Writer $ ((), x) 

-- ..
-- (v, l) <- listen ..
-- ..

listen :: Monoid w => Writer w a -> Writer w (a, w)
listen (Writer (v, l)) = Writer $ ((v, l), l) 

listens :: Monoid w => (w -> b) -> Writer w a -> Writer w (a, b)
listens f (Writer (v, l)) = Writer $ ((v, f l), l) 

censor :: Monoid w => (w -> w) -> Writer w a -> Writer w a
censor f (Writer (v, l)) = Writer $ (v, f l)

-- Монада State. Определение и всякие вспомогательные функции

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap g (State sa) = State $ \s -> first g $ sa s  

instance Applicative (State s) where
    pure :: a -> State s a
    pure x = State $ \s -> (x, s)
    (<*>) :: State s (a -> b) -> State s a -> State s b
    State sf <*> State sa = State $ \s -> let
        (fun, s') = sf s
        (arg, s'') = sa s'
      in (fun arg, s'')

instance Monad (State s) where
    return :: a -> State s a
    return = pure
    (>>=) :: State s a -> (a -> State s b) -> State s b
    (State x) >>= k = State $ \s -> let
                                (val, s1) = x s
                                in (runState $ k val) s1

execState :: State s a -> s -> s
execState st = snd . runState st

evalState :: State s a -> s -> a
evalState st = fst . runState st

-- do
--  ..
--  s <- get
--  ..
--
get :: State s s
get = State $ \s -> (s, s)

-- do
--  ..
--  put 4
--  ..
put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

gets :: (s -> a) -> State s a
gets f = State $ \s -> (f s, s)

-- Решим для начала простую задачку на нее. Дома вам придется сделать аналогичное
-- Очередной способ посчитать факториал:

fac :: Int -> Integer
fac n = fst $ execState (replicateM_ n facStep) (1, 0)
	where
		facStep :: State (Integer, Integer) ()
		facStep = do
			(f, i) <- get
			put (f * (i + 1), i + 1)

-- через forM_?

facForM :: Integer -> Integer
facForM n = execState (forM_ [0..(n-1)] (\i -> do
  f <- get
  put $ f * (i + 1)
  )) 1

-- А вот теперь более содержательные примеры. В следующих трех заданиях нужно как-то заиспользовать
-- монаду State
-- Напишите функцию, которая ищет в списке первый элемент, который удовлетворяет предикату
-- Кроме того, предикат выстреливает эффектом.

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p = foldr (\x rec -> p x >>= (\flg -> if flg then pure $ Just x else rec)) (pure Nothing) 


examplePredicate :: (Show a, Integral a) => a -> IO Bool
examplePredicate x = do
  putStrLn $ "Got: " <> show x
  pure $ even x


-- Найдите первый повторяющийся элемент в списке.
firstRepeats :: Ord a => [a] -> Maybe a
firstRepeats xs = evalState (findM pred xs) Set.empty
  where
    pred :: Ord a => a -> State (Set.Set a) Bool
    pred x = do
      set <- get 
      if (x `Set.member` set)
      then 
        pure True
      else do
        put $ (x `Set.insert` set)
        pure False

-- Наконец, найдите все повторяющиеся элементы в списке и удалите их
distinct :: Ord a => [a] -> [a]
distinct xs = evalState (filterM pred xs) Set.empty 
	where
		pred x = do
			set <- get
			if (x `Set.member` set)
			then
				pure False
			else do
				put $ (x `Set.insert` set)
				pure True


-- Немного про IORef. Это такая конструкция, которая позволяет вам заводить мутабельные ссылки
-- в монаде IO. Ха-ха, вот вам и мутабельные данные в функциональном языке!
-- Живут в Data.IORef и предоставляют следующий интерфейс для работы:

-- создание
-- newIORef :: a -> IO (IORef a)

-- чтение
-- readIORef :: IORef a -> IO a

-- запись
-- writeIORef :: IORef a -> a -> IO ()

-- изменение
-- modifyIORef :: IORef a -> (a -> a) -> IO ()

-- Строгая версия modifyIORef
-- modifyIORef’ :: IORef a -> (a -> a) -> IO ()

-- Например:
testIORef :: IO [Integer]
testIORef = do
  x <- newIORef 0
  v0 <- readIORef x
  modifyIORef x succ
  v1 <- readIORef x
  modifyIORef x succ
  v2 <- readIORef x
  modifyIORef x succ
  v3 <- readIORef x
  pure [v0, v1, v2, v3]

-- Можно писать в очень близком к императивному стилю
factorial :: Integer -> IO Integer
factorial n = undefined 

-- См ../dummy

-- Про случайные числа

-- В хаскелле есть джва способа получить генератор псевдослучайных чисел
-- Первый -- использовать глобальный, который инициализируется системным временем
-- Функция называется getStdGen и живет в System.Random
-- При каждом запуске програмы -- новая уникальная псевдослучайная последовательность

-- Второй - если нужна воспроизводимость. Называется mkStdGen, живет там же, принимает на
-- вход инициализирующее значение

-- Интерфейс примерно такой:
-- randomIO :: IO a
-- random :: RandomGen g => g -> (a, g)
-- randoms :: RandomGen g => g -> [a]

-- Есть функции для получения случайных чисел в диапазоне
-- randomRIO :: (a, a) -> IO a
-- randomR :: RandomGen g => (a, a) -> g -> (a, g)
-- randomRs :: RandomGen g => (a, a) -> g -> [a]

-- Что бы каждый раз получать разные списки с помощью randoms или randomRs
-- возникает необходимость передавать генератор между вычислениями. Напишите функцию,
-- которая прячет это дело в монаде State


randomRState :: (Random a, RandomGen g) => (a, a) -> State g a
randomRState (lo, hi) = State $ (\g -> (randomR (lo, hi) g))

-- Что бы убедиться, что она работает используйте это:

test :: ([Int],[Int])
test = evalState doWork (mkStdGen 42)

doWork :: State StdGen ([Int], [Int])
doWork = do
    xs <- replicateM 5 $ randomRState (1, 6)
    ys <- replicateM 5 $ randomRState (1, 6)
    return (xs, ys)

-- Если в результате у вас получилось, что функция возвращает разные списки, то все ок
