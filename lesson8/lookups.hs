lookups :: (Eq k) => k -> [(k,v)] -> [v]
lookups x ys = (lookups' x ys) >>= id
lookups' x ys = do 
  (k, v) <- ys
  if k == x
    then pure [v]
    else pure []

