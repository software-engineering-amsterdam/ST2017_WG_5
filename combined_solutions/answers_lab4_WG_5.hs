-- Jordy Bottelier
-- Lars Lokhoff
-- Timo Dobber
-- Dennis Kruidenberg

--Questions assignment 1 (time spent 2 hours)
-- 1. Why can't a set be legitimate and extraordinary at the same time?
-- 2. Why can a pair be defined by (a, b) = {{a}, {a, b}}
-- 3. If (a, b) = {{a}, {a, b}}, is (a, b, c) = {{a}, {a, b}, {a, b, c}}?

-- Assignment 8 time spent(45 minutes): 
-- Consider our domain R = (x, y)
-- Then the transative closure of R = {(x, y)}
-- Symmetric closure is defined as : S = R U {(x, y) : (y, x) -> R}
-- Therefore the symmetric closure of the transative closure of R is {(x, y), (y, x)}
--
-- The symmetric closure of (x, y) is (following our definition) {(x, y), (y, x)}
-- The transative closure of this is: {(x, x), (y, y), (x, y), (y, x)}

-- So to the question:
-- Is there a difference between the symmetric closure of the transitive closure of a relation RR and the transitive closure of the symmetric closure of RR?
-- Yes there is, its the difference between  {(x, y), (y, x)} and {(x, x), (y, y), (x, y), (y, x)}

import Data.List
import Test.QuickCheck
import System.Random
import SetOrd
import Test.QuickCheck.Gen

type Rel a = [(a,a)]

-- Assignment 2 (3 hours) --------------------------------------------------------------------------------------
-- create random integer in interval
genInt :: Int -> Int -> IO Int
genInt a b = getStdRandom (randomR (a,b))

-- Create a list of random length with random numbers
fillList :: Int -> IO [Int]
fillList 0 = return []
fillList n = do
	newInt  <- genInt (-200) 200
	myList <- fillList (n-1)
	return (newInt:myList)

-- First create a list of random length, then turn this into a set
genSetInt :: IO (Set Int)
genSetInt = do
	len <- genInt 1 50
	generated_list <- (fillList len)
	return (list2set generated_list)

-- Use quickcheck to create a set
arbitrarySingleSet :: IO (Set Int)
arbitrarySingleSet = do
    k <- generate arbitrary :: IO [Int]
    return (list2set k)

assignment2 = do
	print "Manually generated set: "
	a <- genSetInt
	print a
	print "QuickCheck generated set: "
	a <- arbitrarySingleSet
	print a


-- Assignment 5 (time: 1 hour) --------------------------------------------------
turnTuple :: Rel a -> Rel a
turnTuple [] = []
turnTuple (x:xs) = x : (snd x, fst x) : (turnTuple xs)

symClos :: Ord a => Rel a -> Rel a
symClos x = sort(nub (turnTuple x))

assignment5 = print (symClos [(1,2),(2,3),(3,4)])

-----------6 2 hours

--fixed point from the lecture code
fp :: Eq a => (a -> a) -> a -> a 
fp f = until (\ x -> x == f x) f


trClos :: Ord a => Rel a -> Rel a 
trClos a= sort(fp (\ b -> (nub) ( b ++ (b @@ b))) a)

-----------7 4 hours

testTrClos:: (Eq a) => Ord a => Rel a  -> Bool
testTrClos a = contains a (trClos a)

testSymClos:: (Eq a) => Ord a => Rel a  -> Bool
testSymClos a = contains a (symClos a)

contains:: Eq a => [a] -> [a] -> Bool
contains a b = all (\x -> elem x b) a 

testerTrClos:: Integral a => Rel a -> Bool
testerTrClos a = testTrClos a

testerSymClos:: Integral a => Rel a -> Bool
testerSymClos a = testSymClos a





