{-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE FlexibleContext #-}

newtype Cmps f g x = Cmps { getCmps :: f (g x) } deriving (Eq,Show)

instance (Functor f, Functor g) => Functor (Cmps f g) where
  fmap f (Cmps fga) = Cmps $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Cmps f g) where
  -- pure :: a -> (Cmps a)
  pure = Cmps . pure . pure
  -- (<*>) :: Cmps f g (a -> b) -> Cmps f g a -> Cmps f g b
  Cmps f <*> Cmps x = Cmps ((<*>) <$> f <*> x)

