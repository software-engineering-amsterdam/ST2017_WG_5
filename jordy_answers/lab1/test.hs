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
--	
--  Exercise 7c: Multiple tests can be implemented to test the validity of the luhn checker. If a large set of known valid and invalid luhn numbers is given,
--	all that needs to be done is to perform the check on these numbers and see wether or not the outcome of the check matches the corresponding value of the
--	luhn number (True or False). Since such a list is not present, we chose to implement a luhn number generator and a non-luhn number generator. 
--	The checker should always return True with the numbers from the luhn generator, and False for the numbers of the non-luhn generator
--	Tests can be run with (first parameter is the random seed, the second parameter the number of tests): 
--	main = print(checkLuhnsFalse 1078593479 1000)
--	main = print(checkLuhns 1078593478 1000)

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
revList digits = reverse (intToList (digits * 10))

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

--add all numbers together and every second number is doubled
prepareComputations2 :: Int -> Int
prepareComputations2 digits = (sum (map sumAndSub (double_2nd (revList digits))))

-- generate luhn numbers by first creating a 15 digit random number, then doubling every other element starting
-- at the last element. Then sum all the elements, and compute what the check digit should be to create a luhn number
createLuhns :: Int -> Int -> [Int]
createLuhns seed num = 
	let
		x = (tenPseudorandomNumbers seed num);
	in zipWith pasteInt x (map getCheckDigit (map prepareComputations2 x))

-- generate non-luhn numbers by first creating a 15 digit random number, then doubling every other element starting
-- at the last element. Then sum all the elements, and compute what the check digit should be, add 1 to it, to create a non-luhn number
createFalseLuhns :: Int -> Int -> [Int]
createFalseLuhns seed num = 
	let
		x = (tenPseudorandomNumbers seed num);
	in zipWith pasteInt2 x (map getCheckDigit (map prepareComputations2 x))

--add the check digit to the other digits
pasteInt :: Int -> Int -> Int
pasteInt giant n = giant * 10 + n

--add the bad check digit to the other digits
pasteInt2 :: Int -> Int -> Int
pasteInt2 giant n = giant * 10 + n + 1

-- compute check digit
getCheckDigit :: Int -> Int
getCheckDigit n =
	if (n `mod` 10) == 0
		then 0
		else 10 - (n `mod` 10)

-- check wether the luhn formula holds by inserting generated luhn numbers, entire array should hold True elements
checkLuhns :: Int -> Int -> [Bool]
checkLuhns seed num = map luhn (createLuhns seed num)

-- check wether the luhn formula holds by inserting generated non-luhn numbers, entire array should hold False elements
checkLuhnsFalse :: Int -> Int -> [Bool]
checkLuhnsFalse seed num = map luhn (createFalseLuhns seed num)

--main = print(checkLuhnsFalse 1078593479 1000)
--main = print(checkLuhns 1078593478 1000)