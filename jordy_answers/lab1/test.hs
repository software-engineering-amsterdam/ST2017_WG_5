-- 	Jordy Bottelier 10747338
--	Dennis Kruidenberg
--	Timo Dobber
--	Lars Lokhoff

--	Answers excercises:

--	Excercise 1.1: Boundaries for quickCheck generator: 0 - 10000. This is done to prevent overflow, and the natural numbers should be positive.

-- 	Exercise 2: The property is not very hard to test, since all that needs to be tested is wether or not the hypothesis holds for lots of cases.
-- 	Since the way to create a powerset is already given, all that needs to be done is create a list of length n, 
--	create the powerset using the subsequence function, calculate the length of this list, and check wether this equals 2 ^ n.
--	Computations on large numbers for this test however take quite a long time. Therefore the boundaries of the test are set from 1 to 20.
--	(We start at 1 since n must be a non negative integer)
--
--	The hypothesis in this case is already proven mathematically. Quickcheck only checks for 100 numbers, not all natural numbers.
--	If we would like to prove the hypothesis, which should hold for all positive natural numbers, we should actually test for all these numbers.
--	Since that is unachievable (calculations to infinity are not possible), the hypothesis can not be confirmed with these tests. The tests can only
--	confirm that the hypothesis is true for the given test cases. If we however assume the hypothis is true, we can test wether or not subsequence
--	satisfies a part of its specification, namely creating a powerset of the correct length.  

import Data.List
import Test.QuickCheck
import System.Random

-- boundaries for exercise 1
gen1 :: Gen Int
gen1 = choose (0,10000)

-- boundaries for exercise 2
gen2 :: Gen Int
gen2 = choose (1,20)

-- random number generator for assignment 7c
gen3 :: Gen Int
gen3 = choose (1000000000000000,999999999999999)

-- exercise 1 -------------------------------------------------------:
right :: Int -> Int
right n = (abs (n * (n + 1) * (2 * n + 1)) ) `div` 6

left :: Int -> Int
left n = sum (map square [1..n])

square :: Int -> Int
square n = n ^ 2

myTestExercise1 :: Int -> Bool
myTestExercise1 n = left n == right n

--main = quickCheck $ forAll gen1 myTestExercise1


-- exercise 2 -------------------------------------------------------:
myTestExercise2 :: Int -> Bool
myTestExercise2 n = 2 ^ n == length (subsequences [1..n])

--main = verboseCheck $ forAll gen2 myTestExercise2



-- exercise 7 a ----------------------- The Luhn Formula --------------:


-- Create a list representation of integers by 'biting off' every first digit using modulo 10.
-- https://stackoverflow.com/questions/3989240/int-int-convert
intToList :: Int -> [Int]
intToList 0 = []
intToList x = intToList (x `div` 10) ++ [x `mod` 10]


-- double every second element in the list:
-- https://stackoverflow.com/questions/17383169/haskell-double-every-2nd-element-in-list
double_2nd :: [Int] -> [Int]
double_2nd [] = []
double_2nd [x] = [x]
double_2nd (x:xs) = x : (2 * head xs) : double_2nd (tail xs)

-- Sum the entire list of which every second element is doubled
prepareComputations :: Int -> Int
prepareComputations digits = (sum (map sumAndSub (double_2nd (tail (revList digits))))) + head (revList digits)

revList :: Int -> [Int]
revList digits = reverse (intToList digits)

sumAndSub :: Int -> Int
sumAndSub n =
	if n > 9
		then n - 9
		else n

-- check if a digit satisfies the luhn formula
luhn :: Int  -> Bool
luhn checkdigits = (prepareComputations checkdigits) `mod` 10 == 0


--main = print (luhn 5519760048192084)

-- exercise 7 b ------------------------------------------------------:

isAmericanExpress :: Int -> Bool
isAmericanExpress digits = ((head (intToList digits) == 3) && ((intToList digits)!!1 `elem` [4,7])) && (luhn digits) && (length (intToList digits) == 15)

isMaster :: Int -> Bool
isMaster digits = (head (intToList digits) == 5) && ((head (tail (intToList digits))) `elem` [1..5]) && (luhn digits) && (length (intToList digits) == 16)

isVisa :: Int -> Bool
isVisa digits = (head (intToList digits) == 4) && (luhn digits) && (length (intToList digits) `elem` [13, 16, 19])

--some test cases, each has one true and one false value
--main = print (isAmericanExpress 378787355568920)
--main = print (isAmericanExpress 5519760048192084)
--main = print (isMaster 5519760048192084)
--main = print (isMaster 4539315692581881)
--main = print (isVisa 4539315692581881)
--main = print (isVisa 378787355568920)

-- exercise 7 c -----------------------------------------------------:
tenPseudorandomNumbers :: Int -> Int -> [Int]
tenPseudorandomNumbers seed num_test = take num_test . randomRs (100000000000000, 999999999999999) . mkStdGen $ seed


prepareComputations2 :: Int -> Int
prepareComputations2 digits = (sum (map sumAndSub (double_2nd (tail (revList digits)))))

createLuhns :: Int -> Int -> [Int]
createLuhns seed num = 
	let
		x = (tenPseudorandomNumbers seed num);
	in zipWith pasteInt (map reverseInt (map getCheckDigit (map prepareComputations2 x))) x

--createLuhns :: Int -> Int -> [Int]
--createLuhns seed num = 
--	let
--		x = (tenPseudorandomNumbers seed num);
--	in zipWith pasteInt (map getCheckDigit (map prepareComputations x)) x

pasteInt :: Int -> Int -> Int
pasteInt giant n = giant * 10 + n

--reverse an integer: https://stackoverflow.com/questions/19725292/how-to-reverse-an-integer-in-haskell
reverseInt :: Int -> Int
reverseInt x = read . reverse . show $ x


getCheckDigit :: Int -> Int
getCheckDigit n =
	if (n `mod` 10) == 0
		then 0
		else 10 - (n `mod` 10)

checkLuhns :: Int -> Int -> [Bool]
checkLuhns seed num = map luhn (createLuhns seed num)

tmp :: Int -> Int -> [Int]
tmp seed num = 
	let
		x = (tenPseudorandomNumbers seed num);
	in map prepareComputations x

main = print(checkLuhns 1078593479 10)
--7992739871
--main = print (double_2nd [7, 9,	9, 2,7,	3,	9,	8,	7,	1])
--main = print (tmp 3 2)
--main = print (getCheckDigit 441977446690719)
--main = print (prepareComputations 441977446690719)