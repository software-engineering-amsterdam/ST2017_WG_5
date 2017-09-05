-- Timo Dobber 10386726
-- Abs is used to only have positive numbers.

module Lab1 where
import Data.List
import Test.QuickCheck

-- Assignment 1a (time: 3 hours)
-- todo Add precondition for x (delete abs)

right :: Int -> Int
right x = (x * (x + 1) * (2 * x + 1)) `div` 6

left :: Int -> Int
left x = sum (list x)

square :: Int -> Int
square x = x * x

list :: Int -> [Int]
list x = map square [1..x]

check :: Int -> Bool
check x = left (abs x) == right (abs x)

-- Assignment 1b (time: 30 minutes)

right2 :: Int -> Int
right2 x = ((x * (x + 1)) `div` 2) * ((x * (x + 1)) `div` 2)

left2 :: Int -> Int
left2 x = sum (list2 x)

third :: Int -> Int
third x = x * x * x

list2 :: Int -> [Int]
list2 x = map third [1..x]

check2 :: Int -> Bool
check2 x = left2 (abs x) == right2 (abs x)

-- Assignment 2
listSize :: Int -> Int
listSize x = length (subsequences [1..x])

power :: Int -> Int
power x = 2 ^ x

check3 :: Int -> Bool
check3 x = listSize (abs x) == power (abs x)

-- Assignment 3

-- Assignment 4 (time: 3 hours)
-- To test this with quicktest, you just check if the random number is a 
-- prime and when that is the case you check if the reversal is a prime.

-- From the lab.
reversal :: Integer -> Integer
reversal = read . reverse . show

-- From the lab.
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

-- From the lab.
primes :: [Integer]
primes = 2 : filter prime [3..10000]

getReverse :: [Integer]
getReverse = map reversal primes

check4 :: [Integer] -> [Integer] -> [Integer]
check4 xs ys = if null xs
                 then ys
                 else if prime (head xs) 
                   then check4 (tail xs) (ys ++ [head xs]) 
                   else check4 (tail xs) ys

-- Assignment 8
data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
accuses Matthew _ = False
accuses Peter Matthew = True
accuses Peter Jack = True
accuses Peter _ = False
accuses Jack Matthew = True
accuses Jack Peter = True
accuses Jack _ = False
accuses Arnold _ = False
accuses Carl Arnold = True
accuses Carl _ = False

checkAccusement :: Boy -> [Boy] -> [Boy] -> [Boy]
checkAccusement x xs ys = if null xs
                            then ys
                            else if accuses (head xs) x
                              then checkAccusement x (tail xs) ([head xs] ++ ys)
                              else checkAccusement x (tail xs) ys

accusers :: Boy -> [Boy]
accusers x = checkAccusement x boys []

--guilty, honest :: [Boy]
--guilty = boys
--honest = boys