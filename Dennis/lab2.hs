-- Recognizing triangles ---------------- 70 min
--I've defined some trangles and used these to check my function
--The list tests contrains quadruples with int, int, int, shape
--In Test every triangle is checked using the all command
import Data.Char (ord)
import System.Random
data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c  
    | a^2 + b^2 == c^2 = Rectangular
    | a == b && b == c && c == a = Equilateral
    | (a == b || b == c || a == c) && (a+b > c) && (b+c > a) && (a+c>b) = Isosceles
    | (a+b > c) && (b+c > a) && (a+c>b) = Other
    | otherwise = NoTriangle

test:: Bool
test = all (\(a,b,c,shape) -> triangle a b c == shape) tests

tests :: [(Integer, Integer, Integer, Shape)]
tests = [(3,4,5,Rectangular),(5,12,13, Rectangular),(3,3,3, Equilateral), 
    (6,6,6,Equilateral), (1,1,1000,NoTriangle),(1,1,-1,NoTriangle), (5,5,3,Isosceles)]

--main = print(test)

-- IBAN -----
iban :: String -> Bool
iban x = 
    let 
        i = replace 4 x
        country = take 2 x
        lengthX = length x
    in toInt(replChar i) `mod` 97 == 1 && getLength country lengthOfIban  == lengthX
  
--https://stackoverflow.com/questions/20667478/haskell-string-int-type-conversion  
toInt::String -> Integer
toInt x = read x :: Integer

--with help from:
--https://stackoverflow.com/questions/23974282/haskell-search-list
getLength:: String -> [(Int, String)] -> Int
getLength x [] = -1
getLength x (y:ys) = if x == snd y then fst y else getLength x ys

replace::Int -> String -> [Char]
replace n x
    | n == 0 = x
    | otherwise = replace (n-1) (tail x ++ [head x])

replChar:: String -> String
replChar [] = []
replChar (x:xs) = if ord(x) >= 65 then show (ord x - 55) ++ replChar xs else x:replChar xs

testCorrectIbans:: Bool
testCorrectIbans = all (\x -> iban x == True) ibans


testFalseIbans:: Bool
testFalseIbans = all (\x -> iban x == False) ibansFalse

main = print(testFalseIbans)

--length of iban and iban found on https://www.dnb.no/en/business/transaction-banking/international-payments/example-iban.html
--Only first 10, would be taken a lot of time otherwise
lengthOfIban = [(24,"AD"),(20,"AT"),(22,"BH"),(16,"BE"),(20,"BA"), (22,"BG"),(21,"HR"),(28,"CY"),(24,"CZ"),(18,"DK"),(20,"EE")]
ibans = ["AD1200012030200359100100", "AT611904300234573201", "BH67BMAG00001299123456", 
    "BE68539007547034", "BA391290079401028494", "BG80BNBG96611020345678", "HR1210010051863000160"
    , "CY17002001280000001200527600", "CZ6508000000192000145399", "DK5000400440116243", "EE382200221020145685"]
--false iban, by changing the length or a number at the end. 
ibansFalse = ["AD1200012030200359100101", "AT611904300234573202", "BH67BMAG00001299123453", 
    "BE68539007547035", "BA391290079401028496", "BG80BNBG96611020345677", "HR1210010051863000168"
    , "CY17002001280000001200527606", "CZ6508000000192000145395", "DK5000400440116244", "EE382200221020145684"]


--main = print(replace 4 "GB82WEST12345698765432")