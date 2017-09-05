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

-- exercise 1 -------------------------------------------------------:
right :: Int -> Int
right n = (abs (n * (n + 1) * (2 * n + 1)) ) `div` 6

left :: Int -> Int
left n = sum (map square [1..n])

square :: Int -> Int
square n = n ^ 2

myTestExercise1 :: Int -> Bool
myTestExercise1 n = left n == right n

main = quickCheck $ forAll gen1 myTestExercise1
--main = verboseCheck $ forAll gen1 myTestExercise1


-- exercise 2 -------------------------------------------------------:
myTestExercise2 :: Int -> Bool
myTestExercise2 n = 2 ^ n == length (subsequences [1..n])

--main = verboseCheck $ forAll gen2 myTestExercise2
