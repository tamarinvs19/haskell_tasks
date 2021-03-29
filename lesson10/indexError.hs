{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.Except

data ListIndexError = 
  ErrTooLargeIndex Int 
  | ErrNegativeIndex 
  | OtherErr String
  deriving (Eq, Show)

infixl 9 !!!


(!!!) :: MonadError ListIndexError m => [a] -> Int -> m a
[] !!! n = if n < 0 
  then (throwError ErrNegativeIndex)
  else (throwError (ErrTooLargeIndex n))

xs !!! n | n == 0 = return $ head xs
         | n < 0  = throwError ErrNegativeIndex
         | otherwise = ((tail xs) !!! (n - 1)) `catchError` f
          
f (ErrTooLargeIndex n) = throwError $ ErrTooLargeIndex (n+1)
f (ErrNegativeIndex) = throwError ErrNegativeIndex

