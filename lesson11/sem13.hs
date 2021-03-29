{-# LANGUAGE FlexibleContexts #-}

module Sem13 where

import Control.Monad.Except
import Control.Monad.State
import Control.Arrow
import Data.Monoid
import Data.Maybe
import Data.List
import Data.Char


-- Вывод типов.
-- Сегодня будет не самая обычная практика по модулю того, что я почти ничего не буду говорить. Нам нужно будет написать алгоритм вывода типов для просто типизированного лямбда-исчисления.
-- Для этого нам понадобятся некоторые определения:
-- Далее, везде, где написан undefined необходимо написать реализацию

infixl 2 :@
infixr 3 :->

type Symb = String

-- Терм
data Expr =
    Var Symb
    | Expr :@ Expr
    | Lam Symb Expr
    deriving (Eq,Show,Read)

-- Тип
data Type =
    TVar Symb
    | Type :-> Type
    deriving (Eq,Show)

-- Контекст
newtype Env = Env [(Symb, Type)]
    deriving (Eq,Show)

-- Подстановка
newtype SubsTy = SubsTy [(Symb, Type)]
    deriving (Eq,Show)

-- Всякие вспомогательные штуки
-- Свободные переменные терма(уже писали в большой домашке)
freeVars :: Expr -> [Symb]
freeVars (Var v) = [v]
freeVars (a :@ b) = freeVars a `union` freeVars b
freeVars (Lam v e) = freeVars e \\ [v]

-- Свободные типовые переменные. Есть ли в STT связанные переменные в типах?
freeTVars :: Type -> [Symb]
freeTVars (TVar v) = [v]
freeTVars (t1 :-> t2) = (freeTVars t1) `union` (freeTVars t2 )

-- Расширение контекста
extendEnv :: Env -> Symb -> Type -> Env
extendEnv (Env ls) symb t = Env (ls `union` [(symb, t)])

-- Свободные типовые переменные в контексте
freeTVarsEnv :: Env -> [Symb]
freeTVarsEnv (Env ls) = foldl union [] [freeTVars $ snd l | l <- ls]

-- Контекст можно рассматривать еще и как частичную функцию из множества переменных во множество типов
appEnv :: (MonadError String m) => Env -> Symb -> m Type
appEnv (Env xs) v = if fs == [] then throwError $ template v else return (snd $ head fs)
  where
    fs = filter (\pair -> fst pair == v) xs

template :: Symb -> String
template s = "There is no variable \"" ++ s ++ "\" in the enviroment."

-- Подстановку можно применять к типу
appSubsTy :: SubsTy -> Type -> Type
appSubsTy (SubsTy []) t = t
appSubsTy (SubsTy (l:ls)) t@(TVar v) = 
  if (fst l) == v
  then (snd l) 
  else (appSubsTy (SubsTy ls) t)
appSubsTy sty (t1 :-> t2) = (appSubsTy sty t1) :-> (appSubsTy sty t2)

-- И к контексту
appSubsEnv :: SubsTy -> Env -> Env
appSubsEnv ls (Env (e:es)) = 
  let (Env n) = (appSubsEnv ls (Env es)) in
      Env ((fst e, (appSubsTy ls (snd e))):n)
appSubsEnv ls e = e

-- Их можно комбинировать
composeSubsTy :: SubsTy -> SubsTy -> SubsTy
composeSubsTy st1@(SubsTy s1) st2@(SubsTy s2) = 
  SubsTy $ map (applyST st1 . applyST st2) [(t, TVar t) | t <- types]
  where applyST st (v, t) = (v, appSubsTy st t)
        types = fst (unzip s1) `union` fst (unzip s2)

-- И подстановки образуют моноид
instance Semigroup SubsTy where
  (<>) = composeSubsTy

instance Monoid SubsTy where
  mempty = SubsTy []

-- Наконец, реализуйте алгоритм унификации для двух типов
unify :: (MonadError String m) => Type -> Type -> m SubsTy
unify t1@(TVar v1) t2@(TVar v2) 
  | v1 == v2 = return mempty
  | otherwise = return $ SubsTy [(v1, t2)]
unify (TVar v) t 
  | v `elem` freeTVars t = throwError $ "Can't unify (TVar " ++ show v ++ ") with (" ++ show t ++ ")!"
  | otherwise = return $ SubsTy [(v, t)]
unify st1@(t1 :-> t2) st2@(TVar v) = unify st2 st1
unify (t1 :-> t2) (t3 :-> t4) = do
  x2 <- unify t2 t4
  x1 <- unify (appSubsTy x2 t1) (appSubsTy x2 t3)
  return $ x1 `mappend` x2

-- Реализуйте алгоритм составления системы ограничений для терма в заданном контексте и начальном типе. Обратите особое внимание на случаи аппликации и абстракции
equations :: (MonadError String m) => Env -> Expr -> Type -> m [(Type,Type)]
equations env expr t = evalStateT (equationsState env expr t) "a"

equationsState :: (MonadError String m) => Env -> Expr -> Type -> StateT String m [(Type,Type)]
equationsState env (Var x) t = do
  res <- appEnv env x
  return $ [(t, res)]
equationsState env (m :@ n) t = do
  alpha <- getFreshTVar
  ms <- equationsState env m (alpha :-> t)
  ns <- equationsState env n alpha
  return $ ms ++ ns
equationsState env (Lam x m) t = do
  alpha <- getFreshTVar
  beta <- getFreshTVar
  xs <- equationsState (extendEnv env x alpha) m beta 
  return $ xs ++ [(alpha :-> beta, t)]

getFreshTVar :: MonadState String m => m Type
getFreshTVar = do
  n <- get
  modify (++ "'")
  return $ TVar n


-- Воспользовавшись им, напишите алгоритм поиска главной пары -- главного типа и контекста, в котором верно утверждение о типизации данного терма
principlePair :: (MonadError String m) => Expr -> m (Env,Type)
principlePair expr = do
  let sigma0 = TVar "kek"
  let tVars0 = map (TVar . ("a" ++) . show) [0..]
  let env0 = Env $ zip (freeVars expr) tVars0
  (e, t) <- liftM mergeEquations $ equations env0 expr sigma0
  subst <- unify e t
  return (appSubsEnv subst env0, appSubsTy subst sigma0)

mergeEquations :: [(Type,Type)] -> (Type,Type)
mergeEquations  = foldr1 (\(e1, t1) (e2, t2) -> (e1 :-> e2, t1 :-> t2))

