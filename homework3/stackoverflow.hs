data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr (\x tree -> insertInTree x tree) Leaf


insertInTree :: a -> Tree a -> Tree a
insertInTree x Leaf = Node 0 (Leaf) x (Leaf)
insertInTree x (Node n t1 val t2)
    | h1 < h2   = Node  n (insertInTree x t1) val t2 
    | h1 > h2   = Node  n    t1 val t2n 
    | otherwise = Node (h+1) t1 val t2n
    where h1  = heightTree t1
          h2  = heightTree t2
          t2n = insertInTree x t2
          h   = heightTree t2n     -- might stay the same


heightTree :: Tree a -> Integer
heightTree Leaf = 0
heightTree (Node n t1 val t2) = n