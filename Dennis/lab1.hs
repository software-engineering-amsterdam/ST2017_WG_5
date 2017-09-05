-- Dennis Kruidenberg


----- 1 -------(3 hours)

import Data.List
import Test.QuickCheck
import System.Random

gen1 :: Gen Int
gen1 = choose (1,10)


rightHand2 :: Int -> Int
rightHand2 n = (n * (n + 1) * (2 * n +1))  `div` 6

tmp2 :: Int -> [Int]
tmp2 n = map (^2) [1..n]

leftHand2:: Int -> Int
leftHand2 n = sum (tmp2 n)

test2 :: Int ->  Bool
test2 n = leftHand2 n == rightHand2 n
        



rightHand3 :: Int -> Int
rightHand3 n = (n * (n + 1) `div` 2)^2

tmp3 :: Int -> [Int]
tmp3 n = map (^3) [1..n]

leftHand3:: Int -> Int
leftHand3 n = sum (tmp3 n)

test3 :: Int ->  Bool
test3 n = leftHand3 n == rightHand3 n



--main = verboseCheck $ forAll gen1 test3


---------3 from the workshop (10min)
perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
     insrt x [] = [[x]]
     insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

test4 :: Int -> Bool
test4 n = length(perms([1..n])) == product([1..n])
--main = quickCheck $forAll gen1 test4


-----6---- 1 hour
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
      where xs = takeWhile (\ y -> y^2 <= n) primes
primes :: [Integer]
primes = 2 : filter prime [3..] 

notprimes = filter (not . prime) (map ((+1) . product) [take n primes | n <- [1..10]])
main = print(notprimes)

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
revList digits = reverse (intToList digits)

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
tenPseudorandomNumbers :: Int -> Int -> [Int]
tenPseudorandomNumbers seed num_test = take num_test . randomRs (100000000000000, 999999999999999) . mkStdGen $ seed

createLuhns :: Int -> Int -> [Int]
createLuhns seed num = zipWith (+) x (map getCheckDigit (map prepareComputations x))
                        where x = tenPseudorandomNumbers seed num

checkLunes



getCheckDigit :: Int -> Int
getCheckDigit n =
    if (n `mod` 10) == 0
        then 0
        else 10 - (n `mod` 10)

main = print(zipWith (+) [100,200] [10,20])





























