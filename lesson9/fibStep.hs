import Control.Monad.State
fib :: Int -> Integer
fib n = fst $ execState (replicateM n fibStep) (0,1)

fibStep :: State (Integer,Integer) ()
fibStep = do 
  (f1, f2) <- get
  put (f2, f1 + f2)

