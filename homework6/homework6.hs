-- Exercise 1

fib::Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

x1 = map fib [0..100]


fibx::Integer -> Integer
fibx 0 = 0
fibx 1 = 1
fibx n = multifib 2 0 1
    where 
        multifib i prev current
            |i==n = current
            |otherwise = multifib (i+1) current (prev + current)
            
x2 = map fibx [0..100]



--excercise2

-- natural number example

-- nats::[Int]

-- nats = cons 0
--     where cons n = n:cons (n+1)


-- data Tree a = Empty
--             | Fib 0
--             | Fib 1
--             | Node (Fib (n-1)) Fib n (Tree Fib(n-2))
--     deriving Foldable


