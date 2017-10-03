module Lab4 where 
import Data.List
import Test.QuickCheck
import Lecture4
import SetOrd

-- Exercise 3, Time: 7 hours

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

-- Exercise 4, time: 2 hours
-- Why is an asymmetric relation always anitsymmetric?
-- What is the reflexive transitive closure of R = {(n, n + 1) | n ∈ N} ?

