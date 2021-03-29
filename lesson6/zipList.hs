import Control.Applicative (ZipList(ZipList), getZipList)

(>*<) :: [a -> b] -> [a] -> [b]
(>*<) list_f list_a = getZipList $ ZipList list_f <*> ZipList list_a

(>$<) :: (a -> b) -> [a] -> [b]
(>$<) f list_a = getZipList $ f <$> ZipList list_a
infixl 4 >$<
infixl 4 >*<

