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
main = testUniform 10000