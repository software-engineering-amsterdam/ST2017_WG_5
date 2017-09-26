-- Jordy Bottelier
-- Lars Lokhoff
-- Timo Dobber
-- Dennis Kruidenberg


import Data.List
import Test.QuickCheck
import System.Random
import SetOrd
import Test.QuickCheck.Gen


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

-- Assignment 5 (time: 1 hour)
turnTuple :: Rel a -> Rel a
turnTuple [] = []
turnTuple (x:xs) = x : (snd x, fst x) : (turnTuple xs)

symClos :: Ord a => Rel a -> Rel a
symClos x = sort(nub (turnTuple x))