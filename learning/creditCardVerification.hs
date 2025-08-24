
joinIntegerList :: [Integer] -> [Integer] ->[Integer]
joinIntegerList [] y = y
joinIntegerList (x:[]) y = x:y
joinIntegerList (x:x2:(zs)) y = x:x2:(joinIntegerList zs y)

reverseListOfInteger :: [Integer] -> [Integer]
reverseListOfInteger [] = []
reverseListOfInteger (x:[]) = [x]
reverseListOfInteger (x:(y:(zs))) = joinIntegerList (reverseListOfInteger zs) [y,x]


data Person = Employee 
    | Student 
    | Owner

convertToString :: Person -> String
convertToString Employee = "Employee"
convertToString Student = "Student"
convertToString Owner = "Owner"



sumDigits :: Integer -> Integer
sumDigits x
    |x`div`10 == 0 = x
    |x`div`10 < 10 = x`mod`10 + x`div`10
    |x`div`10 > 1 = x`mod`10 + sumDigits (x`div`10)

ifMultiDigitReplaceSumDigit :: [Integer] -> [Integer]
ifMultiDigitReplaceSumDigit [] = []
ifMultiDigitReplaceSumDigit (x:[]) = [sumDigits x]
ifMultiDigitReplaceSumDigit (x:y) = joinIntegerList [sumDigits x] (ifMultiDigitReplaceSumDigit y)

doubleEverySecondDigit :: [Integer] -> [Integer]
doubleEverySecondDigit [] = []
doubleEverySecondDigit (x:[]) = [x]
doubleEverySecondDigit (x:(y:(zs))) =  joinIntegerList (joinIntegerList [x] [2*y]) (doubleEverySecondDigit zs)

sumIntegersInList :: [Integer] -> Integer
sumIntegersInList [] = 0
sumIntegersInList (x:[]) = x
sumIntegersInList (x:(y:(zs))) = x + y + (sumIntegersInList zs)

convertIntegerToListOfDigits :: Integer -> [Integer]
convertIntegerToListOfDigits x
    |(x `div` 10)<1 =[x]
    |(x `div` 10)>=1 = joinIntegerList (convertIntegerToListOfDigits (x `div` 10)) [(x`mod`10)]


validationPreOPeration :: Integer -> Integer
validationPreOPeration x = sumIntegersInList (ifMultiDigitReplaceSumDigit (doubleEverySecondDigit (reverseListOfInteger (convertIntegerToListOfDigits x))))

checkMultipleOf10 :: Integer -> Bool
checkMultipleOf10 x
    |True = x `mod` 10==0

validateCreditCard :: Integer -> Bool
validateCreditCard x = checkMultipleOf10 (validationPreOPeration x)

