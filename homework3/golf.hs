module Golf where



--Exercise 1 Hopscotch
skips :: [a] -> [[a]]
skips l = map groupBy (zip [0,1..(length l -1)] (repeat l))

--returns true if given element is nth element
isNthElem:: (Int,(Int,a)) -> Bool
isNthElem (every,(pos,elem))
    |(pos `mod` (every+1) == 0) = True
    |otherwise = False

--fetches the element from nested constructors
returnElem::(Int, (Int, a)) -> a
returnElem (_, (_, elem)) = elem

--fetches every nth element
geteverynth :: Int -> [(Int,a)] -> [a]
geteverynth n l =map returnElem (filter isNthElem (zip (repeat n)  l))

-- groups every nth element starting from every element to every other element to every n(lenth of the given liet minus 1) element
groupBy:: (Int, [a]) -> [a]
groupBy (n, l) = geteverynth n (zip [1,2..] l)


-- local Maxima


localMaximaTruthy :: [Integer] -> Bool
localMaximaTruthy (x:y:z:r)
    | y>z && x<y = True
    |otherwise = False
localMaximaTruthy _ = False


localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:r)
    |localMaximaTruthy (x:y:z:r) = [y] ++ localMaxima (y:z:r)
    |otherwise =  localMaxima (y:z:r)
localMaxima _ = []

--chatgpt version
localMaximav2 :: [Integer] -> [Integer]
localMaximav2 (x:y:z:r)
    | y > z && x < y = y : localMaxima (y:z:r)
    | otherwise = localMaxima (y:z:r)
localMaximav2 _ = []


--Folding with trees
data Tree a = Leaf|
    Node Integer (Tree a) a (Tree a)
    deriving Show

    
-- foldTree :: [a] -> Tree a
-- foldTree = ...

-- example
-- foldTree "ABCDEFGHIJ" ==
-- Node 3
--  (Node 2
--      (Node 0 
--          Leaf 
--          ’F’ 
--          Leaf)
--      ’I’
--      (Node 1 
--          (Node 0 
--              Leaf 
--              ’B’ 
--              Leaf) 
--          ’C’ 
--          Leaf))
--  ’J’
-- (Node 2
--      (Node 1 
--          (Node 0 
--              Leaf 
--              ’A’ 
--              Leaf) 
--          ’G’ 
--          Leaf)
--      ’H’
--      (Node 1 
--          (Node 0 
--              Leaf 
--              ’D’ 
--              Leaf) 
--          ’E’ 
--          Leaf))

-- constructTree:: a -> Tree a
-- constructTree a = Node 0 Leaf a Leaf

-- -- foldTree :: [a] -> Tree a
-- -- foldTree = 


nodeConstructor::a -> Tree a
nodeConstructor c = Node 0 Leaf c Leaf

isleaf:: Tree a -> Bool
isleaf Leaf = True
isleaf (Node _ _ _ _) = False

-- findleaflevel:: Tree a -> Int
-- findleaflevel

treeConstructor:: Tree a -> Tree a -> Tree a
treeConstructor (Node nodelvl Leaf character Leaf) tree = (Node (nodelvl+1) tree character Leaf)
treeConstructor (Node nodelvl Leaf character rt) tree = (Node (nodelvl) tree character rt)
treeConstructor (Node nodelvl lt character Leaf) tree = (Node (nodelvl) lt character tree)
treeConstructor (Node nodelvl (Node ltLvl llt lchar lrt) character (Node rtLvl rlt rchar rrt)) tree
    |ltLvl<rtLvl = Node (nodelvl) (treeConstructor (Node (ltLvl) llt lchar lrt) tree) character (Node rtLvl rlt rchar rrt)
    |rtLvl<ltLvl = Node (nodelvl) (Node ltLvl llt lchar lrt) character  (treeConstructor (Node (rtLvl) rlt rchar rrt) tree)
    -- |rtLvl==ltLvl = Node (nodelvl) (treeConstructor (Node (ltLvl) llt lchar lrt) tree) character (Node rtLvl rlt rchar rrt)
-- treeConstructor (Node nodelvl (Node ltLvl llt lchar lrt) character (Node rtLvl rlt rchar rrt)) tree
    |all isleaf [llt, lrt, rlt, rrt] = Node (nodelvl+1) (treeConstructor (Node (ltLvl+1) llt lchar lrt) tree) character (Node rtLvl rlt rchar rrt)
    |otherwise = Node (nodelvl+1) (Node (ltLvl+1) llt lchar (treeConstructor lrt tree))  character (Node rtLvl rlt rchar rrt) 




foldTreel :: [a] -> Tree a
foldTreel (first:rest) = foldl treeConstructor (nodeConstructor first) (map nodeConstructor rest)

