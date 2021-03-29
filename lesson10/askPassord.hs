import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (msum)
import Data.Char (isNumber, isPunctuation)
import Control.Monad (guard)

--  Не снимайте комментарий - эти объявления даны в вызывающем коде
newtype PwdError = PwdError String

type PwdErrorIOMonad = ExceptT PwdError IO

askPassword :: PwdErrorIOMonad ()
askPassword = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Storing in database..."

instance Semigroup PwdError where
  (PwdError e1) <> (PwdError e2) = PwdError (e1 ++ e2)

instance Monoid PwdError where
  mempty = PwdError ""

type LengthMonad = ExceptT String IO
getValidPassword :: PwdErrorIOMonad String
getValidPassword = do
  s <- liftIO getLine
  let check = getAnswer s 
  if check /= ""
  then do
    liftIO $ putStrLn check
    guard (isValid s)
  else guard (isValid s)
  return s

getAnswer :: String -> String
getAnswer s = 
  if length s >= 8
      then if any isNumber s
           then if any isPunctuation s
                then ""
                else "Incorrect input:! password must contain some punctuations"
           else "Incorrect input: password must contain some digits!"
      else "Incorrect input: password is too short!"

isValid :: String -> Bool
isValid s = length s >= 8
            && any isNumber s
            && any isPunctuation s

