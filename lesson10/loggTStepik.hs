import Control.Monad.Identity
import Control.Monad
import Control.Applicative

data Logged a = Logged String a deriving (Eq,Show)

newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }


instance Monad m => Monad (LoggT m) where
  return x = LoggT $ return (Logged "" x)
  (LoggT x) >>= k = LoggT $ do
    la <- x
    let mb = out la
    lb <- runLoggT mb
    new <- runLoggT mb
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
