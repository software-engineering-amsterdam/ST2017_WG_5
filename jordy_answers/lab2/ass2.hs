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
import Data.List
import Test.QuickCheck

import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.STRef

infix 1 //> 

(//>) :: Bool -> Bool -> Bool
p //> q = (not p) || q

-- | Randomly shuffle a list without the IO Monad
--   /O(N)/
shuffle :: [a] -> StdGen -> ([a],StdGen)
shuffle xs gen = runST (do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- liftM (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        gen' <- readSTRef g
        return (xs',gen'))
  where
    n = length xs
    newArray :: Int -> [a] -> ST s (STArray s Int a)
    newArray n xs =  newListArray (1,n) xs


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



-- Assignment 4 (3 hours): After creating the isPermutation test, we can now define some properties to test for this function.
-- Properties to test:
-- Where a = [1, 2, 3]
-- b = [3, 2, 1]
-- c = [1, 3, 2]

-----------------------------------------------------------------------------------------------------------------
-- Symmetry: isPermutation a b == isPermutation b a

-- Commutativity: (isPermutation a b AND isPermutation a  c) AND isPermutation b c ==
-- (isPermutation a c AND isPermutation b c) AND isPermutation a b

-- Reflexive: isPermutation a a == isPermutation a a

-- Transitivety: if isPermutation a b AND isPermutation a c THEN isPermutation b c
------------------------------------------------------------------------------------------------------------------

-- We can generate our own permutation lists if we can assume there are no duplicates in the list.
-- To test the properties, we simply get a random list, create 1 or 2 permutations, and check the predicates
-- defined above (the property definition). If the properties hold, then all tests should return true. 

-- Property order (from strongest to weakest):
--		Distributivity
--		Transitivety
--		Commutativity
--		Symmetry
--		Reflexive
-----------------------------------------------------------------------------------------------------------------
-- We automated the tests using quickCheck, every test yields True which means the defined properties are correct.
-----------------------------------------------------------------------------------------------------------------
-- This function tests wether list 1 is a permutation of list 2, it checks for each element in list 1 if it is present
-- in list 2. it then removes both elements from the lists and continues checking, if one list is empty and the other one
-- isnt, we failed, if both lists are empty, we succeded and a permutation is found
isPermutation [] [_] = False
isPermutation [_] [] = False
isPermutation [] [] = True
isPermutation list1 list2 = 
	do
		let x = head list1
		case elemIndex x list2 of 
			Just n -> do
						let a = tail list1
						let b = let (ys,zs) = splitAt n list2   in   ys ++ (tail zs)
						isPermutation a b
			Nothing -> False

-- The following functions are used to test the defined properties, they should return true if it holds for a test case.
-- We also test wether or not the permutations are actual permutations of the original list (which they are since we create
-- them ourselves)

-- Symmetry: isPermutation a b == isPermutation b a
prop_symm :: [Int] -> Bool
prop_symm a = let
	b = fst ( shuffle a (mkStdGen (head a)))
	in ((isPermutation a b == isPermutation b a) //> (isPermutation a b))


-- Commutativity: (isPermutation a b AND isPermutation a  c) AND isPermutation b c ==
-- (isPermutation a c AND isPermutation b c) AND isPermutation a b
prop_comm :: [Int] -> Bool
prop_comm a = let
	b = fst ( shuffle a (mkStdGen (head a)))
	c = fst ( shuffle a (mkStdGen (head b)))
	in (((isPermutation a b && isPermutation a c) && isPermutation b c) && ((isPermutation a c && isPermutation b c) && isPermutation a b) //> (isPermutation a b))


-- Reflexive: isPermutation a a == isPermutation a a
prop_ref :: [Int] -> Bool
prop_ref a = let
	b = fst ( shuffle a (mkStdGen (head a)))
	in ((isPermutation b b && isPermutation b a) //> (isPermutation b b))

-- Transitivety: if isPermutation a b AND isPermutation a c THEN isPermutation b c
prop_trans :: [Int] -> Bool
prop_trans a = let
	b = fst ( shuffle a (mkStdGen (head a)))
	c = fst ( shuffle a (mkStdGen (head b)))
	in (((isPermutation a b && isPermutation c a) && isPermutation b c) //> (isPermutation a b))

--main = do
--	print ("Symmetry Property:")
--	quickCheck prop_symm
--	print ("Commutativity Property:")
--	quickCheck prop_comm
--	print ("Reflexivity Property:")
--	quickCheck prop_ref
--	print ("Transetivity Property:")
--	quickCheck prop_trans