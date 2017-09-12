-- Jordy Bottelier
-- Lars Kolhoff
-- Timo Dobber
-- Dennis Kruidenberg

-- Assignment 1 (time spent: 2.5 hours): To test wether or not the distribution of the pseudo-random numbers is even, 10000 random floats are generated between
-- the interval of 1 and 0. Every float is checked for its value, and we count how many floats occur in the interval of each quartile:
-- (0..0.25),[0.25..0.5),[0.5..0.75),[0.75..1)
-- To count how many floats are in each quartile, we maintain a list which holds the counted values, for every value encountered we raise
-- the value at index 0, 1, 2 or 3 depending on the value of the float and in which quartile it belongs. Eventually the percentage of occurences
-- is calculated and printed

-- Results: Some samples of the results; [25.099998,24.88,25.78,24.24], [24.429998,24.98,25.21,25.380001]. If the distribution of random numbers
-- is uniform, all the output numbers should be close to 25, which they are (the maximum deviation found is 0.8 percent). This means we can
-- conclude that the distribution of the random float generator that was provided is uniform.


import Test.QuickCheck
import System.Random
import Data.Char (ord)

-- Random float generator, generates numbers between 1 and 0
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
			p <- getStdRandom random
			ps <- probs (n-1) 
			return (p:ps)

-- Raise the value at index n in the countlist, to keep track of the number of floats in each quartile
raiseIndex :: [Int] -> Int -> [Int]
raiseIndex myList index = 
	let
		a = myList !! index
		b = a + 1	
	in replaceNth index b myList

-- Replace the N th value of a list with a new value (used by raiseIndex)
-- https://stackoverflow.com/questions/5852722/replace-individual-list-elements-in-haskell
replaceNth n newVal (x:xs)
	| n == 0 = newVal:xs
	| otherwise = x:replaceNth (n-1) newVal xs

-- Find the index of the floats in a quartile
getIndex :: Float -> Int
getIndex n
	| n <= 0.25 = 0
	| n > 0.25 && n <= 0.5 = 1
	| n > 0.5 && n <= 0.75 = 2
	| n > 0.75 && n <= 1 = 3

-- Count the number of floats ocurring in each quartile and return the result
counter :: [Float] -> [Int] -> [Int]
counter [] countList = countList
counter floatList countList =
	let
		index = getIndex (head floatList)
	in counter (tail floatList) (raiseIndex countList index)

percent :: Int -> Int -> Float
percent total num = (fromIntegral num) / ( fromIntegral total) * 100


-- Get a list of floats and count the occurence of the floats within each quartile to check wether the
-- distribution of the random float generator is uniform, a uniform result should look something like this
-- [2500, 2500, 2500, 2500]
testUniform :: Int -> IO ()
testUniform n =
	do
		a <- (probs n)
		let b = (counter a [0,0,0,0])
		let f = [n, n, n, n]
		let c = zipWith (percent) f b
		print c

--main :: IO ()
--main = testUniform 10000


---assignment 2--------------------------------------------
---assignment 3------Recognizing triangl------------------- 70 min
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
---assignment 4--------------------------------------------
---assignment 5--------------------------------------------
---assignment 6---------IBAN-------------------------------4 hours
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
--Only first 10, would have taken a lot of time otherwise
lengthOfIban = [(24,"AD"),(20,"AT"),(22,"BH"),(16,"BE"),(20,"BA"), (22,"BG"),(21,"HR"),(28,"CY"),(24,"CZ"),(18,"DK"),(20,"EE")]
ibans = ["AD1200012030200359100100", "AT611904300234573201", "BH67BMAG00001299123456", 
    "BE68539007547034", "BA391290079401028494", "BG80BNBG96611020345678", "HR1210010051863000160"
    , "CY17002001280000001200527600", "CZ6508000000192000145399", "DK5000400440116243", "EE382200221020145685"]
--false iban, by changing the length or a number at the end. 
ibansFalse = ["AD1200012030200359100101", "AT611904300234573202", "BH67BMAG00001299123453", 
    "BE68539007547035", "BA391290079401028496", "BG80BNBG96611020345677", "HR1210010051863000168"
    , "CY17002001280000001200527606", "CZ6508000000192000145395", "DK5000400440116244", "EE382200221020145684"]