module Lab1 where
import Data.List
import Data.Foldable
import Test.QuickCheck
import Test.QuickCheck.Modifiers

-- Make sure quickCheck works with positive numbers
genA :: Gen Int
genA = choose (1,10)

-- Exercise 1, Worshop 2
right2 :: Int -> Int
right2 n = (n*(n+1)*(2*n + 1)) `div` 6

left2 :: Int -> Int
left2 x = sum (list2 x)

square2 :: Int -> Int
square2 x = x * x

list2 :: Int -> [Int]
list2 l = map square2 [1..l]

compare2 :: Int -> Bool
compare2 x = if (right2 x) == (left2 x) then True else False


-- Exercise 1, Workshop 3
right3 :: Int -> Int
right3 n = (n*(n+1) `div` 2) * (n*(n+1) `div` 2)

left3 :: Int -> Int
left3 x = sum (list3 x)

square3 :: Int -> Int
square3 x = x * x * x

list3 :: Int -> [Int]
list3 l = map square3 [1..l]

compare3 :: Int -> Bool
compare3 x = if (right3 x) == (left3 x) then True else False


-- Exercise 2, Workshop 4
funcSeq :: Int -> Int
funcSeq x = 2^x

subLength :: Int -> Int
subLength x = length (subsequences [1..x])

compare4 :: Int -> Bool
compare4 x = if (subLength x) == (funcSeq x) then True else False

-- Exercise 3, Workshop 5
factorial:: Int -> Int
factorial x = product [1..x]

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
    insrt x [] = [[x]]
    insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

compare5 :: Int -> Bool
compare5 x = if length (perms [1..x]) == (factorial x) then True else False


-- Exercise 5
-- To test if you answer is correct you could check all the sums of 101 primes smaller than the answer. But 
-- because we get our answer by computing these sums in the first way that would be doing double work.
prime :: Int -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Int]
primes = 2 : filter prime [3..550]

-- Find the next prime
nextPrime :: Int -> Int
nextPrime x = if (prime (x + 1)) then (x + 1) else nextPrime (x + 1)

-- Check if the sum of the 101 primes is also a prime. If yes we are done,
-- if no we try again with the lowest prime discarted and a new highest prime found
findSum ::[Int] -> Int
findSum list = if prime (sum list) then (sum list) else findSum ((tail list) ++ [(nextPrime (last list))])

startFinding :: Int
startFinding = findSum (primes)

-- Exercise 8
data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq, Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
accuses Matthew x = not (x == Matthew) && not (x == Carl) 
accuses Peter x = x == Matthew || x == Jack
accuses Jack x = not (accuses Matthew x) && not (accuses Peter x)
accuses Arnold  x = accuses Matthew x /= accuses Peter x
accuses Carl x = not (accuses Arnold x)

checkAccusement :: Boy -> [Boy] -> [Boy] -> [Boy]
checkAccusement x xs ys = if null xs
                            then ys
                            else if accuses (head xs) x
                            then checkAccusement x (tail xs) ([head xs] ++ ys)
                            else checkAccusement x (tail xs) ys

accusers :: Boy -> [Boy]
accusers x = checkAccusement x boys []

-- Because there is only one possibility where 3 people are correct according to the 
-- teacher the boy with 3 accusers will be found guilty. So we check for each person how 
-- many accusers he has untill we find the one with 3 accusers.
checkGuilty :: Boy -> [Boy] -> [Boy]
checkGuilty x xs = if length (accusers x) == 3 then [x] else checkGuilty (head xs) (tail xs)

guilty :: [Boy]
guilty = checkGuilty (head boys) (tail boys)

-- Because only one person is guilty, 3 people who are accusing are correctly  
honest :: [Boy]
honest = accusers (head guilty)





