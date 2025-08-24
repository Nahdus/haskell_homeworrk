import Parser


data ExprT = Lit Integer
    | Add ExprT ExprT
    | Mul ExprT ExprT
    deriving (Show, Eq)


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

evalStr :: [Char] -> Maybe Integer
evalStr s
    |checkifjust x = Just (eval (getJust x))
    |otherwise = Nothing
    where x = (parseExp Lit Add Mul s)

--class Expr
--methods lit add mul

class Expr a where
    add:: a -> a -> a
    mul:: a -> a -> a
    

lit = Lit



instance Expr ExprT where
    add x y = Add x y
    mul x y = Mul x y
    
    