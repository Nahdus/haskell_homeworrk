class SpecialClass a where
    methodOne::Integer -> a
    methodTwo::a -> a -> a


data SpecialType = Good Integer
    | Bad
    deriving Show

instance SpecialClass SpecialType where
    methodOne x = Good x
    methodTwo x y = Bad


instance SpecialClass (Integer -> SpecialType -> SpecialType) where
    methodOne x _ = mulbyGood x
    methodTwo x y = x

mulbyGood::Integer -> SpecialType -> SpecialType
mulbyGood x (Good y) = Good (x * y)
mulbyGood x (Bad) = Bad

dumbDumb::SpecialType -> Integer
dumbDumb (Good x) = x

instance SpecialClass (SpecialType -> Integer) where
    methodOne x _ = dumbDumb (Good x)
    methodTwo x y = x

instance SpecialClass (String -> SpecialType) where
    methodOne x y = subrub "hi"
    methodTwo x y = x

gumbGumb::Integer -> SpecialType
gumbGumb x = Good x

subrub::String -> SpecialType
subrub x
    |x=="hi" = Good 1000
subrub _ = Good 0