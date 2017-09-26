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
<<<<<<< HEAD
import Data.Char (ord)
=======
import Data.Char

>>>>>>> b1e9f90c0757e0f565b80cdc8c8f0a45db0a8daa
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.STRef


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



---assignment 2------Recognizing triangl------------------- 70 min
data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c  
    | a^2 + b^2 == c^2 = Rectangular
    | a == b && b == c && c == a = Equilateral
    | (a == b || b == c || a == c) && (a+b > c) && (b+c > a) && (a+c>b) = Isosceles
    | (a+b > c) && (b+c > a) && (a+c>b) = Other
    | otherwise = NoTriangle

test:: Bool
test = all (\(a,b,c,shape) -> triangle a b c == shape) tests

tests :: [(Integer, Integer, Integer, Shape)]
tests = [(3,4,5,Rectangular),(5,12,13, Rectangular),(3,3,3, Equilateral), 
    (6,6,6,Equilateral), (1,1,1000,NoTriangle),(1,1,-1,NoTriangle), (5,5,3,Isosceles)]

---assignment 3-------------------------------------------- (8 hours, mostly figuring out how to properly sort the properties.)
-- From the labs
<<<<<<< HEAD
infix 1 //> 
=======
>>>>>>> b1e9f90c0757e0f565b80cdc8c8f0a45db0a8daa

(//>) :: Bool -> Bool -> Bool
p //> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- From the labs.
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x //> q x)
weaker   xs p q = stronger xs q p 

-- The four properties from the workshop.
first :: Int -> Bool
first x = (even x && x > 3)

second :: Int -> Bool
second x = (even x || x > 3)

third :: Int -> Bool
third x = ((even x && x > 3) || even x)

-- same as third?
--fourth :: Int -> Bool
--fourth x = ((even x && x > 3) || even x)

-- Using bubblesort to sort the list with the strongest property first and 
-- weakest property last.
-- source: https://smthngsmwhr.wordpress.com/2012/11/09/sorting-algorithms-in-haskell/
bubblesort'iter :: [((Int -> Bool), String)] -> [((Int -> Bool), String)]
bubblesort'iter (x:y:xs)
    | (stronger [(-10)..10] (fst x) (fst y)) = x : bubblesort'iter (y:xs)
    | otherwise = y : bubblesort'iter (x:xs)
bubblesort'iter (x) = (x)

bubblesort' :: [((Int -> Bool), String)] -> Int -> [((Int -> Bool), String)]
bubblesort' xs i 
    | i == (length xs) = xs
    | otherwise = bubblesort' (bubblesort'iter xs) (i + 1) 
 
bubblesort :: [((Int -> Bool), String)] -> [((Int -> Bool), String)]
bubblesort xs = bubblesort' xs 0

-- Uncomment the main function to get the list of properties, strongest first.
--main :: [String]
--main = map snd (bubblesort [(first, "first"), (second, "second"), (third, "third"), (even, "even")])


---assignment 4--------------------------------------------

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

-------------------------------------------------------------------
-- Recognizing and generating derangements, time: 6 hours
-- Properties: 
--      Reflexive: isDerangement [1,2,3][3,1,2] = isDerangement [1,2,3][3,1,2]
--      Commutative: isDerangement [1,2,3][3,1,2] = isDerangement [3,1,2][1,2,3]
-- Well chosen integer lists:
--      Empty list [] -> will not succeed because there are no elements
--      List with 1 element [1] -> will not succeed because there are no permutations
--      List with the same integeres [2,2,2] -> will not succeed because all permutations are still the same lists
-- List of properties strong -> weak:
-- commutative, reflexive     
<<<<<<< HEAD

=======
infix 1 //> 
>>>>>>> b1e9f90c0757e0f565b80cdc8c8f0a45db0a8daa

-- The reflexive property, checked with quickCheck
reflexive_prop :: [Int] -> [Int] -> Bool
reflexive_prop x y = isDerangement x y //> isDerangement x y

-- The commutative property, checked with quickCheck
commutative_prop :: [Int] -> [Int] -> Bool
commutative_prop x y = isDerangement x y //> isDerangement y x

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
    insrt x [] = [[x]]
    insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

-- Loop over all elements in both lists and check if they are the same for each position
isDerangement2 :: [Int] -> [Int] -> Bool
isDerangement2  [] [] = True
isDerangement2 (x:xs) (y:ys) = if x == y 
                            then False 
                            else if null xs || null ys
                            then isDerangement2 xs ys
                            else True

-- Check if the 2 lists are perms, if yes check if it is a derangement
isDerangement :: [Int] -> [Int] -> Bool
isDerangement xs ys = if isPermutation xs ys
                        then isDerangement2 xs ys
                        else False

-- Loop over permutations and check for each if it is a derangement
findDerans2 :: [Int] -> [[Int]] -> [[Int]] -> [[Int]]
findDerans2 original perms derans = if null perms
                                    then derans
                                    else if isDerangement original (head perms)
                                    then findDerans2 original (tail perms) (derans ++ [(head perms)])
                                    else findDerans2 original (tail perms) derans

-- Keep looking for the first derangement, if its found add to list and continue with
-- function above untill all perms are checked
findDerans :: [Int] -> [[Int]] -> [[Int]]
findDerans original perms = if null perms
                                    then [[0]]
                                    else if isDerangement original (head perms)
                                    then findDerans2 original (tail perms) [] ++ [(head perms)]
                                    else findDerans original (tail perms)

-- To find the derangements, create the permutations of the list and start looking!
deran :: Int -> [[Int]]
deran x = findDerans [1..(x - 1)] (perms [1..(x - 1)])

----------------------------------------------------------------
-- ROT13, time: 5 hours
-- Specification: {string str} str2 = encode str {rot13 str2} 
-- To encode a string with rot13 we simply, depending on if the letter is 
-- in caps or not, substract the ascii value of the A or a, add 13, mod 26 and then add the ascii
encode :: String -> String
encode [] = []
encode (x:xs) = if (ord(x) >= 65 && ord(x) <= 90)
                  then [chr ((((ord x - 65) + 13) `mod` 26) + 65)] ++ encode xs
                else if (ord(x) >= 97 && ord(x) <= 122)
                  then [chr ((((ord x - 97) + 13) `mod` 26) + 97)] ++ encode xs
                  else x:encode xs

-- To check if the encodes string is encoded correctly, we have to compare the encoded string
-- with the original string and check if the ascii value is 13 larger (mode 26 because we start at the
-- beginning of the alphabet if we reach the end).
prop_rot13 :: String -> String -> Bool
prop_rot13 "" "" = True
prop_rot13 (x:xs) (y:ys) = if (ord y) == (((((ord x - 65) + 13) `mod` 26) + 65)) 
                            then prop_rot13 xs ys
                            else if (ord y) == ((((ord x - 97) + 13) `mod` 26) + 97)
                            then prop_rot13 xs ys
                            else False

---assignment 6---------IBAN-------------------------------4 hours
iban :: String -> Bool
iban x = 
    let 
        i = replace 4 x
        country = take 2 x
        lengthX = length x
    in toInt(replChar i) `mod` 97 == 1 && getLength country lengthOfIban  == lengthX
  
--https://stackoverflow.com/questions/20667478/haskell-string-int-type-conversion  
toInt::String -> Integer
toInt x = read x :: Integer

--with help from:
--https://stackoverflow.com/questions/23974282/haskell-search-list
getLength:: String -> [(Int, String)] -> Int
getLength x [] = -1
getLength x (y:ys) = if x == snd y then fst y else getLength x ys

replace::Int -> String -> [Char]
replace n x
    | n == 0 = x
    | otherwise = replace (n-1) (tail x ++ [head x])

replChar:: String -> String
replChar [] = []
replChar (x:xs) = if ord(x) >= 65 then show (ord x - 55) ++ replChar xs else x:replChar xs

testCorrectIbans:: Bool
testCorrectIbans = all (\x -> iban x == True) ibans


testFalseIbans:: Bool
testFalseIbans = all (\x -> iban x == False) ibansFalse

main = print(testFalseIbans)

--length of iban and iban found on https://www.dnb.no/en/business/transaction-banking/international-payments/example-iban.html
--Only first 10, would have taken a lot of time otherwise
lengthOfIban = [(24,"AD"),(20,"AT"),(22,"BH"),(16,"BE"),(20,"BA"), (22,"BG"),(21,"HR"),(28,"CY"),(24,"CZ"),(18,"DK"),(20,"EE")]
ibans = ["AD1200012030200359100100", "AT611904300234573201", "BH67BMAG00001299123456", 
    "BE68539007547034", "BA391290079401028494", "BG80BNBG96611020345678", "HR1210010051863000160"
    , "CY17002001280000001200527600", "CZ6508000000192000145399", "DK5000400440116243", "EE382200221020145685"]
--false iban, by changing the length or a number at the end. 
ibansFalse = ["AD1200012030200359100101", "AT611904300234573202", "BH67BMAG00001299123453", 
    "BE68539007547035", "BA391290079401028496", "BG80BNBG96611020345677", "HR1210010051863000168"
    , "CY17002001280000001200527606", "CZ6508000000192000145395", "DK5000400440116244", "EE382200221020145684"]
