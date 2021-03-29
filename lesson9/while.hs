import Data.IORef

while :: IORef a -> (a -> Bool) -> IO () -> IO ()
while ref p action = do 
  i <- readIORef ref
  if p i == True
  then do 
    action
    while ref p action
  else return ()

factorial :: Integer -> IO Integer
factorial n = do
  r <- newIORef 1
  i <- newIORef 1
  while i (<= n) ( do
    ival <- readIORef i
    modifyIORef' r (* ival)
    modifyIORef' i (+ 1)
   )
  readIORef r

