--Anonymous functions

greaterThan100_2 :: [Integer] -> [Integer]
greaterThan100_2 xs = filter (\x -> x > 100) xs

greaterThan100_3 :: [Integer] -> [Integer]
greaterThan100_3 xs = filter (>100) xs

--Function composition

foo :: (b -> c) -> (a -> b) -> (a -> c)
foo f g = \x -> f (g x)

myTest :: [Integer] -> Bool
myTest xs = even (length (greaterThan100_3 xs))

-- (.) operator
myTest2 :: [Integer] -> Bool
myTest2 = even . length . greaterThan100_3

--Currying and partial application

f :: Int -> Int -> Int
f x y = 2*x + y


f'' :: (Int,Int) -> Int
f'' (x,y) = 2*x + y

schönfinkel :: ((a,b) -> c) -> a -> b -> c
schönfinkel f x y = f (x,y)

unschönfinkel :: (a -> b -> c) -> (a,b) -> c
unschönfinkel f (x,y) = f x y

--point-free style

foobar' :: [Integer] -> Integer
foobar' = sum . map (\x -> 7*x + 2) . filter (>3)

--folds
sum' :: [Integer] -> Integer
sum' []     = 0
sum' (x:xs) = x + sum' xs

product' :: [Integer] -> Integer
product' [] = 1
product' (x:xs) = x * product' xs

length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs

fold:: b -> (a -> b -> b) -> [a] -> b
fold z f []     = z
fold z f (x:xs) = f x (fold z f xs)

-- fold f z [a,b,c] == a `f` (b `f` (c `f` z))

sum''     = fold 0 (+)
product'' = fold 1 (*)
length''  = fold 0 (\_ s -> 1 + s)
lengthv2 = fold 0 (\_ -> (1+))
lengthv3 = fold 0 (const (1+))