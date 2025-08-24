-- f :: a -> a -> a
-- f x y = x && y
-- doesn't work because of rigit types

-- f :: a -> a -> a
-- f a1 a2 = case (typeOf a1) of
--     Int  -> a1 + a2
--     Bool -> a1 && a2
--     _    -> a1
-- still does not work


-- :t (+)
-- (+) :: Num a => a -> a -> a


-- (==) :: Eq a   => a -> a -> Bool
-- (<)  :: Ord a  => a -> a -> Bool
-- show :: Show a => a -> String


-- example 
-- f:: a -> a -> A
-- f x y = x && y

-- will throw error
-- Couldn't match type `a' with `Bool'
--       `a' is a rigid type variable bound by
--           the type signature for f :: a -> a -> a at 2012-02-09.lhs:37:3
--     In the second argument of `(&&)', namely `y'
--     In the expression: x && y
--     In an equation for `f': f x y = x && y

-- because it offers caller to choose any type but implementor enforces boolean


-- parametric polymorphism
-- works uniformly not case by case basis

-- Imagine 
-- f a1 a2 = case (typeOf a1) of
--           Int  -> a1 + a2
--           Bool -> a1 && a2
--           _    -> a1
-- This is not possiable in haskell because haskell has parametric polymorphism

--Parametric polymorphism
--parametric polymorphism should work uniformly for any type chosen by caller


--Type classes
-- Num ord Show are Type classes
-- and (==) (>) (<) (+) are "Type-class polymorphic"
-- Type classes correspond to set of types which have certain operations defined for them
-- Type class polymorphic function only work for function that are instance of type classes in question


--reading type classes

-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool

-- Eq is declared to be a type class with single parameter a
-- Any type a which wants to be instance of eq must define the two function (==) and (/=) with the indicated type signature


--standard type classes

-- Ord is for types whose elements can be totally ordered, that is, where any two elements can be compared to see which is less than the other. 
-- It provides comparison operations like (<) and (<=), and also the compare function.

-- Num is for “numeric” types, which support things like addition, subtraction, and multipication. 
-- One very important thing to note is that integer literals are actually type class polymorphic:

-- Prelude> :t 5
-- 5 :: Num a => a
-- This means that literals like 5 can be used as Ints, Integers, Doubles, or any other type which is an instance of Num (Rational, Complex Double, or even a type you define…)

-- Show defines the method show, which is used to convert values into Strings.

-- Read is the dual of Show.

-- Integral represents whole number types such as Int and Integer.

constant :: a -> b ->a
constant a _ = a

unitList::[a] -> [a]
unitList x = x

compose::(b -> c) -> (a -> b) -> (a -> c)
compose f g = f . g

map::(a -> a) -> a -> a
map f a = f a


-- (==) :: Eq a   => a -> a -> Bool
-- (<)  :: Ord a  => a -> a -> Bool
-- show :: Show a => a -> String

-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool

--example

data Foo = F Int | G Char

instance Eq Foo where
  (F i1) == (F i2) = i1 == i2
  (G c1) == (G c2) = c1 == c2
  _ == _ = False

  --Eq actually implemented like this
--   class Eq a where
--   (==), (/=) :: a -> a -> Bool
--   x == y = not (x /= y)
--   x /= y = not (x == y)
-- one needs to declare any oune either (x == y) or  (x /= y) the other one is free

-- but eq is so special that it can be directly derived

-- data Foo' = F' Int | G' Char
--   deriving (Eq, Ord, Show)


-- Standard type classes

-- Here are some other standard type classes you should know about:

-- Ord is for types whose elements can be totally ordered, that is, where any two elements can be compared to see which is less than the other. It provides comparison operations like (<) and (<=), and also the compare function.

-- Num is for “numeric” types, which support things like addition, subtraction, and multipication. One very important thing to note is that integer literals are actually type class polymorphic:

-- Prelude> :t 5
-- 5 :: Num a => a
-- This means that literals like 5 can be used as Ints, Integers, Doubles, or any other type which is an instance of Num (Rational, Complex Double, or even a type you define…)

-- Show defines the method show, which is used to convert values into Strings.

-- Read is the dual of Show.

-- Integral represents whole number types such as Int and Integer.


-- A type class example

-- As an example of making our own type class, consider the following:

class Listable a where
  toList :: a -> [Int]


instance Listable Int where
  -- toList :: Int -> [Int]
  toList x = [x]

exampleInt = toList (10::Int)

instance Listable [Int] where
  toList = id

instance Listable Char where
    toList 'a' = [0]
    toList 'b' = [1]
    toList 'c' = [2]
    toList _ = [100]

instance Listable Bool where
    toList True = [1]
    toList False = [0]

instance (Listable a, Listable b) => Listable (a,b) where
  toList (x,y) = toList x ++ toList y


data Tree a = Empty | Node a (Tree a) (Tree a)

instance Listable (Tree Int) where
  toList Empty        = []
  toList (Node x l r) = toList l ++ [x] ++ toList r

suml x = sum (toList x)

foo x y = sum (toList x) == sum (toList y) || x < y

greaterThan x y = x>y


-- Type classes

--New Type
