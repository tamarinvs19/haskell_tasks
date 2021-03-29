{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.Functor.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Fail
import Control.Monad
import Data.Maybe
import Control.Applicative

data Logged a = Logged String a deriving (Eq,Show)
newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }

instance Monad m => Monad (LoggT m) where
  return x = LoggT $ return (Logged "" x)
  (LoggT x) >>= k = LoggT $ do
    la <- x
    new <- runLoggT (out la)
    return $ addLog new la
    where 
      out (Logged s a) = k a
      getLog (Logged s a) = s
      addLog (Logged newS b) la = Logged ((getLog la) ++ newS) b
  
  fail msg = LoggT (Control.Monad.fail msg)

fp f (Logged s x) = Logged s (f x)
instance Monad m => Functor (LoggT m) where
  fmap f (LoggT x) = LoggT $ do
    x' <- x
    pure $ (fp f x')

instance Monad m => Applicative (LoggT m) where
  pure x = LoggT $ pure (Logged "" x)
  (LoggT x1) <*> (LoggT x2) = LoggT $ do 
    a <- x1
    b <- x2
    pure $ (\(Logged s1 a') (Logged s2 b') -> (Logged (s1++s2) (a' b'))) a b


write2log :: Monad m => String -> LoggT m ()
write2log s = LoggT $ return (Logged s ())

type Logg = LoggT Identity

runLogg :: Logg a -> Logged a
runLogg la = (\(Identity x) -> x) $ runLoggT la 

instance MonadTrans LoggT where
  lift ma = LoggT $ fmap (\x -> Logged "" x) ma

instance MonadState s m => MonadState s (LoggT m) where
  get   = state (\s -> (s, s)) 
  put s = state (\_ -> ((), s))
  state f = lift $ do
      s <- get
      let ~(a, s') = f s
      put s'
      return a

instance MonadReader r m => MonadReader r (LoggT m) where
  ask    = lift $ reader id
  local = mapLoggT . local
  reader f = lift $ do
      r <- ask
      return (f r)

mapLoggT :: (m (Logged a) -> n (Logged b)) -> LoggT m a -> LoggT n b
mapLoggT f a = LoggT $ f (runLoggT a)

class Monad m => MonadLogg m where
  w2log :: String -> m ()
  logg :: Logged a -> m a

instance Monad m => MonadLogg (LoggT m) where
  w2log = write2log
  logg log = LoggT $ return log

instance MonadLogg m => MonadLogg (StateT s m) where
  w2log = lift . w2log
  logg  = lift . logg

instance MonadLogg m => MonadLogg (ReaderT r m) where
  w2log = lift . w2log
  logg  = lift . logg

rdrStLog :: ReaderT Integer (StateT Integer Logg) Integer
rdrStLog = do
  x <- logg $ Logged "BEGIN " 1
  y <- ask
  modify (+ (x+y))
  a <- get
  w2log $ show $ a * 10
  put 42
  w2log " END"
  return $ a * 100

logSt'' :: LoggT (State Integer) Integer
logSt'' = do
  x <- logg $ Logged "BEGIN " 1
  modify (+x)
  a <- get
  w2log $ show $ a * 10
  put 42
  w2log " END"
  return $ a * 100


