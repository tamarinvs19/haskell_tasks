import Control.Monad.Identity
import Control.Monad.Fail
import Control.Monad
import Control.Applicative
import Control.Monad.State

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

logSt :: LoggT (State Integer) Integer
logSt = do
  lift $ modify (+1)
  a <- lift get
  write2log $ show $ a * 10
  lift $ put 42
  return $ a * 100
