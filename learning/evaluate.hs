sumtorial:: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial(n - 1)

op = sumtorial 3

{-
return True if even
return False if negative
-}
oddEven:: Integer -> Bool
oddEven n
    | n `mod` 2 == 0 = True
    |otherwise = False


op1 = oddEven 1


--return True if positive
--return False if negative
--return true if zero
posNeg:: Integer -> Bool
posNeg n
    |n>0 = True
    |n<0 = False
    |True = True

n1 = posNeg (-5)
p1 = posNeg 5
z1 = posNeg 0

--multi argument

f:: Int -> Int -> Int -> Int
f x y z = x + y + z

c=10
sum3 = f 10 (c+10) 5


--pairs

p::(Int,Char)
p = (3,'x')

--pairs

sumPair :: (Int,Int) -> Int
sumPair (x, y) = x + y

sp = sumPair (1,3)


--lists
nums, range ,lists ::[Integer]
nums = [1,2,3]
range = [1,3..19]
lists = [1..100]

hello1::[Char]
hello1 = ['h','e','l','l','o']

hello2::String
hello2 = "hello"

tof = hello1==hello2

--constructing list

emptylist = []
ex1 = 1:[]
ex2 = 3:(1:[])
ex3 = 2 : 3 : 4 : []



list_generator :: Integer ->[Integer]
list_generator n = n:[]

push_list :: Integer -> [Integer] ->[Integer]
push_list m n = m:n

prevInt :: Integer -> [Integer]
prevInt n
    |n>0 = push_list n (prevInt (n-1))
    |True = []

simplePrevInt :: Integer -> [Integer]
simplePrevInt n = [n,n-1..1]
    

hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3*n + 1
--generate hailstone sequence
hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)


--function list
intListLength :: [Integer] -> Integer
intListLength [] = 0
intListLength (x:xs) = 1 + intListLength xs

sumevery2 :: [Integer] -> [Integer]
sumevery2 [] = []
sumevery2 (x:[]) = [x]
sumevery2 (x:(y:zs)) = (x + y) : sumevery2 zs


--enumeration type
data Thing = Shoe
    |Ship
    |SealingWax
    |Cabbage
    |King
    deriving Show

-- shoe::Thing
-- shoe = Shoe

ship::Thing
ship = Ship

--putting in a  list
listO'Things::[Thing]
listO'Things = [King,Ship,Cabbage]


--pattern matching
isSmall :: Thing -> Bool
isSmall King = False
isSmall Ship = False
isSmall Shoe = True
isSmall SealingWax = True
isSmall Cabbage = True


isSmall2 :: Thing -> Bool
isSmall2 King = False
isSmall2 Ship = False
isSmall2 _ = True



--algebriac data
data FailableDouble = Failure
    |OK Double
    deriving Show


exf = Failure
exok = OK 3.4


safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x/y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d) = d


data Person = Person String Int Thing
    deriving Show

experson = Person "Sudhan" 30 Shoe

sudhan :: Person
sudhan = experson

getAge::Person -> Int
getAge (Person _ age _) = age

exage = getAge experson


--algebriac data type
data AlgDataType  = Constr1  Int Int
    |Constr2  Int
    |Constr3  Int Int Int
    |Constr4 
    


exampleOfAlgDataType::AlgDataType
exampleOfAlgDataType = Constr4

type Type11 = Int
type Type12 = Int
type Type21 = String
type Type31 = String
type Type32 = Int
type Type33 = String

data AlgDataType2  = Constructor1  Type11 Type12
    |Constructor2  Type21
    |Constructor3  Type31 Type32 Type33
    |Constructor4
    

--pattern matching
-- foo (Constr1 a b)   = ...
-- foo (Constr2 a)     = ...
-- foo (Constr3 a b c) = ...
-- foo Constr4         = ...


baz :: Person -> String
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

checkFav :: Person -> String
checkFav p@(Person n _ Shoe) = n ++ ", you're my kind of person!"
checkFav p@(Person n _ _) = n ++ ", your favorite thing is lame."



-- failureToZero :: FailableDouble -> Double
-- failureToZero Failure = 0
-- failureToZero (OK d) = d



--case expression
failureToZero2 :: FailableDouble -> Double
failureToZero2 x = case x of
                     Failure -> 0
                     OK d    -> d

excase = case "Hello" of
    ('H':s) -> length s


      
--recursive datatype
data IntList = Empty 
    | Cons Int IntList
    deriving Show

intListProd :: IntList -> Int
intListProd Empty      = 1
intListProd (Cons x l) = x * intListProd l

aocons::IntList
aocons = Cons 10 Empty


res = intListProd aocons

aocons2 = Empty
res2 = intListProd aocons2


data Tree = Leaf Char
          | Node Tree Int Tree
  deriving Show

tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))