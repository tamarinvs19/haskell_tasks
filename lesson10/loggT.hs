import Control.Monad.Identity
import Control.Monad.Fail
import Control.Monad
import Control.Applicative

data Logged a = Logged String a deriving (Eq,Show)

newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }


instance Monad m => Monad (LoggT m) where
  return x = LoggT $ return (Logged "" x)
  (LoggT x) >>= k = LoggT $ do
    la <- x
    let mb = out la
    new <- runLoggT mb
    return $ addLog new la
    where 
      out (Logged s a) = k a
      getLog (Logged s a) = s
      addLog (Logged newS b) la = Logged ((getLog la) ++ newS) b
  
  fail msg = LoggT (Control.Monad.Fail.fail msg)

instance MonadFail m => MonadFail (LoggT m) where
  fail msg = LoggT (Control.Monad.Fail.fail msg)

instance Monad m => Functor (LoggT m) where
  fmap = liftM

instance Monad m => Applicative (LoggT m) where
  pure = return
  (<*>) = ap


logTst :: LoggT Identity Integer
logTst = do
  x <- LoggT $ Identity $ Logged "AAA" 30
  y <- return 10
  z <- LoggT $ Identity $ Logged "BBB" 2
  return $ x + y + z

failTst :: [Integer] -> LoggT [] Integer
failTst xs = do
  5 <- LoggT $ fmap (Logged "") xs
  LoggT [Logged "A" ()]
  return 42
