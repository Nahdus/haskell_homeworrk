-- More polymorphism and type classes

-- data OP a = Empty 
--     | Got a
--     deriving Show

-- instance Monad OP where
--     return x = Got x
--     Empty >>= _ = Empty
--     (Got x) >>= f = f x

-- removeFirstA::String -> OP String
-- removeFirstA "" = Empty
-- removeFirstA (x:rest)
--     |x=='a' = Got rest
--     |otherwise = Empty

-- removeFirstX::String -> OP String
-- removeFirstX "" = Empty
-- removeFirstX (x:rest)
--     |x=='x' = Got rest
--     |otherwise = Empty


-- bindOP :: (a -> OP b) -> OP a -> OP b
-- bindOP f Empty = Empty
-- bindOP f (Got x) = f x

-- combinedFunction :: String -> OP String
-- combinedFunction str = (removeFirstX str)>>=removeFirstA
-- -- removeAThenX word=   removeFirstA =<< (removeFirstX word)


data OP a = Empty 
          | Got a
          deriving Show

instance Functor OP where
    fmap _ Empty = Empty
    fmap f (Got x) = Got (f x)

instance Applicative OP where
    pure = Got
    Empty <*> _ = Empty
    (Got f) <*> x = fmap f x

instance Monad OP where
    Empty >>= _ = Empty
    (Got x) >>= f = f x

removeFirstA :: String -> OP String
removeFirstA "" = Empty
removeFirstA (x:rest)
    | x == 'a' = Got rest
    | otherwise = Got (x:rest)

removeFirstX :: String -> OP String
removeFirstX "" = Empty
removeFirstX (x:rest)
    | x == 'x' = Got rest
    | otherwise = Got (x:rest)

combinedFunction :: String -> OP String
combinedFunction str = removeFirstX str >>= removeFirstA