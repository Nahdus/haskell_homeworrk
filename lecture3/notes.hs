
data IntList = Empty | Cons Int IntList
  deriving Show

exampleList::IntList
exampleList = Cons 1 (Cons (-2) (Cons 3 Empty))


absAll :: IntList -> IntList
absAll Empty       = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)


squareAll :: IntList -> IntList
squareAll Empty       = Empty
squareAll (Cons x xs) = Cons (x*x) (squareAll xs)

abslist = absAll exampleList
sqlist = squareAll exampleList

addone x = x+1
mapIntList::(Int->Int)->IntList->[Int]
mapIntList mapfunc Empty = []
mapIntList mapfunc (Cons x ys) = mapfunc x : mapIntList mapfunc ys

exampleadded = mapIntList addone exampleList

--Polymorphic type
data List t = E | C t (List t)
    deriving Show

lst1 :: List Int
lst1 = C 3 (C 5 E)

lst2 :: List Char
lst2 = C 'a' ( C 'b' E)

--Polymorphic functions

filterList _ E = E
filterList p (C x xs)
  | p x       = C x (filterList p xs)
  | otherwise = filterList p xs

mapList _ E        = E
mapList f (C x xs) = C (f x) (mapList f xs)


doStuff1 :: [Int] -> Int
doStuff1 []  = 0
doStuff1 [_] = 0
doStuff1 xs  = head xs + (head (tail xs)) 
doStuff2 :: [Int] -> Int
doStuff2 []        = 0
doStuff2 [_]       = 0
doStuff2 (x1:x2:_) = x1 + x2

--Writing partial functions

-- data Maybe a = Nothing | Just a

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x



data NonEmptyList a = NEL a [a]

nelToList :: NonEmptyList a -> [a]
nelToList (NEL x xs) = x:xs

listToNel :: [a] -> Maybe (NonEmptyList a)
listToNel []     = Nothing
listToNel (x:xs) = Just $ NEL x xs

headNEL :: NonEmptyList a -> a
headNEL (NEL a _) = a

tailNEL :: NonEmptyList a -> [a]
tailNEL (NEL _ as) = as

exnel = NEL 1 [1,2,3,5]
exneltolist = nelToList exnel