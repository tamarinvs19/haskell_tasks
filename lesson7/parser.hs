{-# LANGUAGE InstanceSigs #-}
import Control.Applicative

newtype Parser a = Parser { apply :: String -> [(a, String)] }

instance Functor Parser where
  -- fmap :: (a -> b) -> (Parser a) -> (Parser b)
  fmap f (Parser old_parser) = Parser new_parser 
    where
      -- new_parser :: String -> [(b, String)]
      new_parser str = convert (old_parser str)
      -- convert :: [(a, String)] -> [(b, String)]
      convert = map (\(x, s) -> (f x, s))

instance Applicative Parser where
  pure a = Parser $ (\s -> [(a, s)])
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) (Parser f) (Parser x) = (Parser g) where
    g str = [(fab xa, xs) | (fab, fs) <- (f str), (xa, xs) <- (x fs)]

instance Alternative Parser where
  empty = Parser (const [])
  (<|>) (Parser x) (Parser y) = (Parser z) where 
    z str = (x str) ++ (y str)
