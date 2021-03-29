surround :: a -> a -> [a] -> [a]
surround x y zs = (surround' x y zs) >>= id

surround' x y zs = do
  z <- zs
  pure [x, z, y]
