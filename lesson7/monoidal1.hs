class Functor f => Monoidal f where
  unit  :: f ()
  (*&*) :: f a -> f b -> f (a,b)

instance Monoidal Maybe where
  unit = Just ()
  (*&*) (Just a) (Just b) = Just (a, b)
  (*&*) _ _ = Nothing

instance Monoid s => Monoidal ((,) s) where
  unit = (mempty, ()) 
  (*&*) (f, a) (f', b) = (f `mappend` f', (a, b))

instance Monoidal ((->) e) where
  unit e = () 
  (*&*) f g e = (f e, g e)
