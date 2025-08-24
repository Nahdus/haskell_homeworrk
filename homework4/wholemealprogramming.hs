fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1v2 :: [Integer] -> Integer
fun1v2 = foldl (*) 1 . map (\e -> (-) e 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

--correct
fun2v2test :: Integer -> Integer
fun2v2test = foldl (+) 0 . filter even . (++[2,0]) . takeWhile (>2) . iterate (\x -> if (x `mod` 2 == 0) then x `div` 2 else (3 * x + 1) ) 

fun2v3 :: Integer -> Integer
fun2v3 1 = 0
fun2v3 n | even n = n + fun2 (n `div` 2) | otherwise = fun2 (3 * n + 1)