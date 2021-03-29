integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = 
  let
    step = (b - a) / 1000
    s x = step * (f(a + step * x) + f(a + (x + 1) * step)) / 2
  in sum(map s [0..999])

