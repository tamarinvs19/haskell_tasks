newtype Matrix a = Matrix [[a]]

showMatrix :: Show a => Matrix a -> ShowS
showMatrix (Matrix (x:[])) = showList x 
showMatrix (Matrix (x:xs)) = showList x . showChar '\n' . showMatrix (Matrix xs)
showMatrix _  = showString "EMPTY"

instance Show a => Show (Matrix a) where
  showsPrec _ = showMatrix 

