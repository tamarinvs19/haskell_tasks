import Control.Monad.Except
import Data.Char

data ParseError = ParseError { location :: Int, reason :: String }

type ParseMonad = Either ParseError

parseHex :: String -> ParseMonad Int --eger
parseHex (c:s) =
  if isHexDigit c
  then case (parseHex s) of
             Left e -> Left (ParseError ((location e) + 1) (reason e))
             Right res -> Right ((digitToInt c) * (16 ^ (length s)) + res)
  else Left (ParseError 1 ([c] ++ ": invalid digit"))
parseHex [] = Right 0

printError :: ParseError -> ParseMonad String        
printError e = Right ("At pos " ++ (show $ location e) ++ ": " ++ (reason e)) 

-- тестирование
test s = str where
  (Right str) = do 
      n <- parseHex s
      return $ show n  
    `catchError` printError
