newtype Cmps f g x = Cmps { getCmps :: f (g x) } deriving (Eq,Show)

instance (Functor f, Functor g) => Functor (Cmps f g) where
  fmap f (Cmps fga) = Cmps $ (fmap . fmap) f fga

instance (Foldable f, Foldable g) => Foldable (Cmps f g) where
  foldMap f (Cmps fgx) = foldMap (foldMap f) fgx

instance (Applicative f, Applicative g) => Applicative (Cmps f g) where
  pure = Cmps . pure . pure
  Cmps f <*> Cmps x = Cmps ((<*>) <$> f <*> x)

instance (Traversable f, Traversable g) => Traversable (Cmps f g) where
  traverse f (Cmps fgx) = Cmps <$> (traverse . traverse) f fgx
