{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

import Control.Monad.Except
import Control.Applicative
import Control.Monad

data Excep a =  Err String | Ok a 
  deriving (Eq, Show)

instance Functor Excep where
  fmap f (Ok x) = Ok (f x)
  fmap f (Err s) = Err s

instance Applicative Excep where
  pure a = Ok a
  (Ok f) <*> x = fmap f x
  (Err s) <*> _ = Err s

instance Alternative Excep where
  empty = Err "Alternative.empty error."
  (Err s) <|> x = x
  m <|> _ = m

instance Monad Excep where
  return = Ok
  (Ok a) >>= f = f a
  (Err s) >>= _ = Err s
  fail _ = Err "Monad.fail error."

instance MonadPlus Excep where
  mzero = empty 
  mplus = (<|>)

instance MonadError String Excep where
  throwError e = Err e
  catchError (Err e) handler = handler e
  catchError x _ = x

-- тестирование
(?/) :: (MonadError String m) 
            => Double -> Double -> m Double
x ?/ 0 = throwError "Division by 0."
x ?/ y = return $ x / y

example :: Double -> Double -> Excep String
example x y = action  `catchError` return where 
  action = do 
    q <- x ?/ y
    guard (q >=0)
    if q  > 100 then do 
      100 <- return q
      undefined
    else 
      return $ show q
