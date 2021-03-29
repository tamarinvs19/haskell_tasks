import Data.Complex

newtype Cmplx = Cmplx (Complex Double) deriving Eq

instance Show Cmplx where
  showsPrec _ (Cmplx (a :+ b)) | b >= 0 = shows a . showString "+i*" . shows b 
                               | otherwise = shows a . showString "-i*" . shows (-b) 

instance Read Cmplx where
  readsPrec _ s = [(Cmplx (re :+ (sgn * im)), tailS)]
		where 
			((re, im'):_) = (reads s) :: [(Double, String)]
			sgn | (im' !! 0) == '+' = 1
					| (im' !! 0) == '-' = (-1)
					| otherwise = 0
			((im, tailS):_) = (reads (snd (splitAt 3 im'))) :: [(Double, String)]

