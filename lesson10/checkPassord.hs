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

instance Monoid PwdError where
  mempty = PwdError ""
instance Semigroup PwdError where
  e <> _ = e

getValidPassword :: PwdErrorIOMonad String
getValidPassword = do
  s <- liftIO getLine
  let (check, msg) = getAnswer s
  if check == False
  then do
    liftIO $ putStrLn msg
    guard False
  else guard True
  return s

getAnswer :: String -> (Bool, String)
getAnswer s =
  if length s < 8
  then (False, "Incorrect input: password is too short!")
  else if not $ any isNumber s
  then (False, "Incorrect input: password must contain some digits!")
  else if not $ any isPunctuation s
  then (False, "Incorrect input: password must contain some punctuations!")
  else (True, "")
