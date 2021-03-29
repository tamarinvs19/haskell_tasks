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
    let mb = out la
    new <- runLoggT mb
    return $ addLog new la
    where 
      out (Logged s a) = k a
      getLog (Logged s a) = s
      addLog (Logged newS b) la = Logged ((getLog la) ++ newS) b

instance MonadFail m => MonadFail (LoggT m) where
  fail msg = LoggT (Control.Monad.Fail.fail msg)

instance Monad m => Functor (LoggT m) where
  fmap = liftM

instance Monad m => Applicative (LoggT m) where
  pure = return
  (<*>) = ap


write2log :: Monad m => String -> LoggT m ()
write2log s = LoggT $ return (Logged s ())

type Logg = LoggT Identity

runLogg :: Logg a -> Logged a
runLogg la = (\(Identity x) -> x) $ runLoggT la 

logTst' :: Logg Integer
logTst' = do
  write2log "AAA"
  write2log "BBB"
  return 42

stLog :: StateT Integer Logg Integer
stLog = do
  modify (+1)
  a <- get
  lift $ write2log $ show $ a * 10
  put 42
  return $ a * 100
