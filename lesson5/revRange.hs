import Data.List (unfoldr)
import Data.Char (chr, ord)
revRange :: (Char,Char) -> [Char]
revRange = unfoldr fun

fun :: (Char, Char) -> Maybe (Char, (Char, Char))
fun (a, b)
	| a > b = Nothing
 | a <= b = Just(b, (a, chr ((ord b) - 1)))
