-- Timo Dobber 10386726
-- Abs is used to only have positive numbers.

module Lab1 where
import Data.List
import Test.QuickCheck

-- Assignment 1a (time: 3 hours)
-- todo Add precondition for x (delete abs)
-- usage: quickCheck check

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
-- usage: quickCheck check1

right2 :: Int -> Int
right2 x = ((x * (x + 1)) `div` 2) * ((x * (x + 1)) `div` 2)

left2 :: Int -> Int
left2 x = sum (list2 x)

third :: Int -> Int
third x = x * x * x

list2 :: Int -> [Int]
list2 x = map third [1..x]

check1 :: Int -> Bool
check1 x = left2 (abs x) == right2 (abs x)

-- Assignment 2 (time: 1 hour)
-- Yes, this property is hard to test because the function is exponential.
-- We test whether subsequences satisfies a part of its specification.
-- usage: quickCheck check2

listSize :: Int -> Int
listSize x = length (subsequences [1..x])

power :: Int -> Int
power x = 2 ^ x

check2 :: Int -> Bool
check2 x = listSize (abs x) == power (abs x)

-- Assignment 3 (time: 30 minutes)
-- This assignment is also hard to test because the function is exponential.
-- usage: quickCheck check3

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

check3 :: Int -> Bool
check3 x = length(perms([1..x])) == product([1..x])

-- Assignment 4 (time: 3 hours)
-- To test this with quicktest, you just check if the random number, which can 
-- not be negative, is a prime and when that is the case you check if the 
-- reversal is also a prime.

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

-- Assignment 8 (time: 4 hours)
-- The lies and accusations can be seen as logic operations.
-- In order to find the guilty boy, we need him to be accused by 3 other boys,
-- because 3 boys always tell the truth according to the teacher. So whenever a 
-- boy gets accused by 3 other boys, those 3 boys speak the truth and the other 
-- 2 lie.

data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
accuses Matthew x = not (x == Matthew) && not (x == Carl)
accuses Peter x = (x == Matthew) || (x == Jack)
accuses Jack x = not (accuses Matthew x) && not (accuses Peter x)
accuses Arnold x = (((accuses Matthew x) || (accuses Peter x)) && (not (accuses Matthew x) && (accuses Peter x)))
accuses Carl x = not (accuses Arnold x)

checkAccusement :: Boy -> [Boy] -> [Boy] -> [Boy]
checkAccusement x xs ys = if null xs
                            then ys
                            else if accuses (head xs) x
                              then checkAccusement x (tail xs) ([head xs] ++ ys)
                              else checkAccusement x (tail xs) ys

accusers :: Boy -> [Boy]
accusers x = checkAccusement x boys []

checkGuilty :: Boy -> [Boy] -> [Boy]
checkGuilty x xs = if length (accusers x) == 3
                    then [x]
                    else checkGuilty (head xs) (tail xs)

liars :: [[Boy]]
liars = filter ((== 2) .length) (subsequences boys)

guilty :: [Boy]
guilty = checkGuilty (head boys) (tail boys)

honest :: [Boy]
honest = accusers (head guilty)