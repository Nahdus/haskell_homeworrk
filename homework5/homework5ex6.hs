{-# LANGUAGE FlexibleInstances #-}

import qualified Data.Map as M
import Prelude

class HasVars a where
    var::String -> a
        
class Expr a where
    lit:: Integer -> a
    add:: a -> a -> a
    mul:: a -> a -> a


data VarExprT = Lit Integer
    | Add VarExprT VarExprT
    | Mul VarExprT VarExprT
    | Var String
    deriving (Show, Eq)

instance HasVars VarExprT where
    var x = Var x

instance Expr VarExprT where
    lit = Lit
    add = Add
    mul = Mul
-- --instance HasVars (M.Map String Integer -> Maybe Integer)
-- instance HasVars (M.Map String Integer -> Maybe Integer) where
--     var x = M.fromList []
-- -- y = M.fromList [(5,"a"), (3,"b")] 

varMapValue::(M.Map String Integer) -> String -> Maybe Integer
varMapValue strInt str
    |M.member str strInt = Just (strInt M.! str)
    |otherwise = Nothing


type VarIntegerMap = (M.Map String Integer -> Maybe Integer)
    

-- instance HasVars VarIntegerMap where
--     var x =  M.lookup x


-- instance Expr VarIntegerMap where
--     lit x = 
--     add x y =
--     mul x y =

-----------------------------------------------------------------------------------------------
-- lit x  =  x
-- Expected: VarIntegerMap
-- Actual: Integer

-- lit x y =  x
-- expected type `Maybe Integer'
-- actual type `Integer'

-- lit x y =  y
-- expected type: Maybe Integer
-- actual type: M.Map String Integer

-- lit x _ =  x
-- expected type `Maybe Integer'
-- actual type `Integer'

-- lit x  =  Just x
-- namely x
-- expected: VarIntegerMap ie M.Map String Integer -> Maybe Integer
-- got: Maybe Integer

-- lit x y =  Just y
-- expected Integer
-- got M.Map String Integer


-----------------------------------------------------------------------------------------------------

instance Expr VarIntegerMap where
    lit x _ = Just x
    add x y = M.lookup "x"
    mul x y = M.lookup "x"



-- -- z = M.fromList[("x",2)]
-- c= M.map ((*) 2 )  (M.fromList [("a",5), ("b", 3)])

-- fc = ((*) 2 )
-- sd = (M.fromList [("a",5), ("b", 3)] M.!)