data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
    deriving Show

foldTree :: [a] -> Tree a
foldTree xs = foldr insertBalanced Leaf (zipWith (\d x -> (x, d)) depths xs)
  where
    depths = balancedDepths (length xs)

balancedDepths :: Int -> [Integer]
balancedDepths n = map (\h -> ceiling (logBase 2 (fromIntegral (n + 1)) - logBase 2 (fromIntegral h))) [0..]

insertBalanced :: (a, Integer) -> Tree a -> Tree a
insertBalanced (x, d) Leaf = Node d Leaf x Leaf
insertBalanced (x, d) (Node curDepth left val right)
  | d <= curDepth = Node curDepth (insertBalanced (x, d) left) val right
  | otherwise     = Node curDepth left val (insertBalanced (x, d - curDepth - 1) right)

