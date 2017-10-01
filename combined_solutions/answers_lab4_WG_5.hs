-- Jordy Bottelier
-- Lars Lokhoff
-- Timo Dobber
-- Dennis Kruidenberg

--Questions assignment 1 (time spent 2 hours)
-- 1. Why can't a set be legitimate and extraordinary at the same time?
-- 2. Why can a pair be defined by (a, b) = {{a}, {a, b}}
-- 3. If (a, b) = {{a}, {a, b}}, is (a, b, c) = {{a}, {a, b}, {a, b, c}}?

-- Assignment 4, time: 2 hours
-- Why is an asymmetric relation always anitsymmetric?
-- What is the reflexive transitive closure of R = {(n, n + 1) | n ∈ N} ?


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

-- Assignment 3, Time: 7 hours

-- We create the intersection by using the list2set function and the build-in intersect function
intersectionSet :: (Ord a) => Set a -> Set a -> Set a
intersectionSet (Set set1) (Set set2) = list2set (intersect set1 set2)

-- We create a union of 2 sets by using list2set and the build-in union function
unionSet2 :: (Ord a) => Set a -> Set a -> Set a 
unionSet2 (Set set1) (Set set2)  = list2set (union set1 set2)

-- We create the difference between set1 and set2 by using list2set and the build-in difference
differenceSet :: (Ord a) => Set a -> Set a -> Set a
differenceSet (Set set1) (Set set2) = list2set (set1 \\ set2)

-- We can check if the union is correct if the difference of the
-- union of set1 and set2 and the differences of set1 and set2 is the same as set2
propUnion1 :: (Ord a) => (Set a, Set a) -> Bool
propUnion1 (set1, set2) = if differenceSet (unionSet2 set1 set2) (differenceSet set1 set2) == set2 then True else False

-- We can check if the union is correct if the difference of the
-- union of set1 and set2 and the differences of set2 and set1 is the same as set1
propUnion2 :: (Ord a) => (Set a, Set a) -> Bool
propUnion2 (set1 , set2) = if differenceSet (unionSet2 set1 set2) (differenceSet set2 set1) == set1 then True else False

-- Check for each element in the intersection (third variable) if it is in both set1 and set2
propIntersectionHelper :: (Ord a) => Set a -> Set a -> Set a -> Bool
propIntersectionHelper _ _ (Set []) = True
propIntersectionHelper set1 set2 (Set (x:xs)) = if (inSet x set1) && (inSet x set2) 
                                                                    then propIntersectionHelper set1 set2 (Set xs)
                                                                    else False
-- A property of the intersection is that everything in the intersection is in both set1 and set2
-- so this is what we check with the help of the helper function above
propIntersection :: (Ord a) => (Set a, Set a) -> Bool
propIntersection (set1, set2) = propIntersectionHelper set1 set2 (intersectionSet set1 set2)

-- Check for each element in the difference if it is in set1 and not in set2
propDifferenceHelper :: (Ord a) => Set a -> Set a -> Set a -> Bool
propDifferenceHelper _ _ (Set []) = True
propDifferenceHelper set1 set2 (Set (x:xs)) = if (inSet x set1) && not (inSet x set2) 
                                                                    then propDifferenceHelper set1 set2 (Set xs)
                                                                    else False
-- Use the helper function to check if everyting in the difference of set1 and set2
-- is in set1 but not is set2
propDifference :: (Ord a) => (Set a, Set a) -> Bool
propDifference (set1, set2) = propDifferenceHelper set1 set2 (differenceSet set1 set2)

-- This function tests the property of a given property using in this case always the
-- generateIntegerSet but we cannot pass that from here
testProp :: Show a => IO a -> (a -> Bool) -> IO ()
testProp generator property = test generator property 0 10

-- This function does 10 tests using randomly generated sets on the given property
-- using the given set generator
test :: Show a => IO a -> (a -> Bool) -> Int -> Int -> IO ()
test setGenerator property run limit = if run == limit 
                                       then print (show run ++ " tests succeeded")
                                       else do 
                                        sets <- setGenerator
                                        if not (property sets)
                                            then print (show run ++ " test failed")
                                            else do
                                                print (show run ++ " test passed")
                                                test setGenerator property (run+1) limit
                                            
-- This returns a random pair of 2 integer sets
arbitraryIntSets :: IO (Set Int, Set Int)
arbitraryIntSets = do
    k <- generate arbitrary :: IO [Int]
    j <- generate arbitrary :: IO [Int]
    return (list2set k, list2set j)

-- This creates a pair using the function given (in our case always arbitraryIntSets)
generateAnySetPair :: IO (a, a) -> IO (a, a) 
generateAnySetPair generater = do
    k <- generater
    return k

-- This generates an pair of 2 integer sets using the functions above
generateIntegerSet :: IO (Set Int, Set Int)
generateIntegerSet = generateAnySetPair arbitraryIntSets


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
--can be run from ghci via GHCI 
--quickCheck testerTrClos
--quickCheck testerSymClos
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





