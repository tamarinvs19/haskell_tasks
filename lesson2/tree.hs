data Tree a = Leaf | Node (Tree a) a (Tree a)

treeSum :: Tree Integer -> Integer
treeSum Leaf = 0
treeSum (Node a b c) = b + (treeSum $ a) + (treeSum $ c)

treeHeight :: Tree a -> Int
treeHeight Leaf = 0
treeHeight (Node a b c) = 1 + (max (treeHeight $ a) (treeHeight $ c))
