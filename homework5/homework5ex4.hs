import Parser



data ExprT = Lit Integer
    | Add ExprT ExprT
    | Mul ExprT ExprT
    deriving (Show, Eq)

--version 1
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add (Lit x) (Lit y)) = x+y
eval (Mul (Lit x) (Lit y)) = x * y
eval (Mul z (Lit x)) = x * (eval z)
eval (Mul (Lit x) z ) = x * (eval z)

-- ans = eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))


checkifjust::Maybe a -> Bool
checkifjust (Just a) = True
checkifjust Nothing = False

getJust::Maybe a -> a
getJust (Just a) = a

--exercise 2
evalStr :: [Char] -> Maybe Integer
evalStr s
    |checkifjust x = Just (eval (getJust x))
    |otherwise = Nothing
    where x = (parseExp Lit Add Mul s)

--class Expr
--methods lit add mul

class Expr a where
    lit:: Integer -> a
    add:: a -> a -> a
    mul:: a -> a -> a
    

instance Expr ExprT where
    lit x = Lit x
    add x y = Add x y
    mul x y = Mul x y

instance Expr Integer where
    lit x = x
    add x y = x + y
    mul x y = x * y

instance Expr Bool where
    lit x = x<1
    add x y = x || y
    mul x y = x && y

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)


instance Expr MinMax where
    lit x = MinMax x
    add (MinMax x) (MinMax y)
        |x<y =  MinMax y
        |otherwise = MinMax x
    mul (MinMax x) (MinMax y)
        |x<y = MinMax x
        |otherwise = MinMax y

instance Expr Mod7 where
    lit x = Mod7 x
    add (Mod7 x) (Mod7 y) = Mod7 ((x+y) `mod` 7)
    mul (Mod7 x) (Mod7 y) = Mod7 ((x*y) `mod` 7)



    
        
        