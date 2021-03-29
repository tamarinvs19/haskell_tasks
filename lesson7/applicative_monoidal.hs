import Control.Applicative(liftA2)

unit' :: Applicative f => f ()
unit' = pure ()

pair' :: Applicative f => f a -> f b -> f (a,b)
pair' = liftA2 (\x y -> (x, y))
