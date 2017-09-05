-- Dennis Kruidenberg


----- 1 -------(3 hours)

import Data.List
import Test.QuickCheck

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


-----6----
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
      where xs = takeWhile (\ y -> y^2 <= n) primes
primes :: [Integer]
primes = 2 : filter prime [3..] 

notprimes = filter (not . prime) (map ((+1) . product) [take n primes | n <- [1..10]])
--main = print(notprimes)

----8----
--https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
digits :: Integer -> [Int]
digits = map (read . return) . show



hi :: [Int] -> Int

hi[x] = x
hi(x:xs) = x + hi xs

main = print(hi[1,2,3,4])






