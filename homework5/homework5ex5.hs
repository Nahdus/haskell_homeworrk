{-# LANGUAGE TypeSynonymInstances #-}
import StackVM
import Parser


class Expr a where
    lit:: Integer -> a
    add:: a -> a -> a
    mul:: a -> a -> a
    


class BoolExpr a where
    litB:: Bool -> a
    or :: a -> a -> a
    and:: a -> a -> a





instance Expr Program where
    lit x = [PushI x]
    add x y = x++y++[Add]
    mul x y = x++y++[Mul]

instance BoolExpr Program where
    litB x = [PushB x]
    or x y = x++y++[Or]
    and x y = x++y++[And]


