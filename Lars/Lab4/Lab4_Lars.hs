module Lab4 where 
import Data.List
import Test.QuickCheck
import Lecture4
import SetOrd

-- Exercise 3, Time: 5 hours

intersectionSet :: (Ord a) => Set a -> Set a -> Set a
intersectionSet (Set set1) (Set set2) = list2set (intersect set1 set2)

-- Union from setOrd
unionSet2 :: (Ord a) => Set a -> Set a -> Set a 
unionSet2 (Set [])     set2  =  set2
unionSet2 (Set (x:xs)) set2  = 
   insertSet x (unionSet2 (Set xs) set2)

differenceSet :: (Ord a) => Set a -> Set a -> Set a
differenceSet (Set set1) (Set set2) = list2set (set1 \\ set2)

propUnion1 :: (Ord a) => (Set a, Set a) -> Bool
propUnion1 (set1, set2) = if differenceSet (unionSet2 set1 set2) (differenceSet set1 set2) == set2 then True else False

propUnion2 :: (Ord a) => (Set a, Set a) -> Bool
propUnion2 (set1 , set2) = if differenceSet (unionSet2 set1 set2) (differenceSet set2 set1) == set1 then True else False

propIntersectionHelper :: (Ord a) => Set a -> Set a -> Set a -> Bool
propIntersectionHelper _ _ (Set []) = True
propIntersectionHelper set1 set2 (Set (x:xs)) = if (inSet x set1) && (inSet x set2) 
                                                                    then propIntersectionHelper set1 set2 (Set xs)
                                                                    else False

propIntersection :: (Ord a) => (Set a, Set a) -> Bool
propIntersection (set1, set2) = propIntersectionHelper set1 set2 (intersectionSet set1 set2)

propDifferenceHelper :: (Ord a) => Set a -> Set a -> Set a -> Bool
propDifferenceHelper _ _ (Set []) = True
propDifferenceHelper set1 set2 (Set (x:xs)) = if (inSet x set1) && not (inSet x set2) 
                                                                    then propDifferenceHelper set1 set2 (Set xs)
                                                                    else False

propDifference :: (Ord a) => (Set a, Set a) -> Bool
propDifference (set1, set2) = propDifferenceHelper set1 set2 (differenceSet set1 set2)

testProp :: Show a => IO a -> (a -> Bool) -> IO ()
testProp generator property = test generator property 0 10

test :: Show a => IO a -> (a -> Bool) -> Int -> Int -> IO ()
test setGenerator property run limit = if run == limit 
                                       then print (show run ++ " tests succeeded")
                                       else do 
                                        sets <- setGenerator
                                        if not (property sets)
                                            then print (show run ++ " test failed")
                                            else do
                                                print (show run ++ " test progressed")
                                                test setGenerator property (run+1) limit
                                            

arbitraryIntSets :: IO (Set Int, Set Int)
arbitraryIntSets = do
    k <- generate arbitrary :: IO [Int]
    j <- generate arbitrary :: IO [Int]
    return (list2set k, list2set j)

generateAnySetPair :: IO (a, a) -> IO (a, a) 
generateAnySetPair generater = do
    k <- generater
    return k

generateIntSet :: IO (Set Int, Set Int)
generateIntSet = generateAnySetPair arbitraryIntSets
