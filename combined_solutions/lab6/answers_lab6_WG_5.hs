
import Lecture6
-- assignment 2 (2 hours) test report, code for assignment 1 can be found in the lecture code:
-- We execute the same tests using both implementations 100.000 times with the arguments:
-- x = 20000
-- y = 43000000
-- n = 301
-- 
-- These variables are chosen simply because they are very large, and it is well known that the algorithm used is
-- more efficient for large exponentional numbers. 
--
-- executing the test (main2) 100.000 times for expM:
-- (8.06 secs, 1.380.801.752 bytes)
--
-- executing the test (main3) 100.000 times for exM:
-- (0.10 secs, 44835960 bytes)
--
-- The first main is used to let haskell load all the packages and not affect the execution time of the other functions
--
-- We can clearly see that our own implementation is a lot faster, namely 80.6 times for such large numbers. 

testExM :: Integer -> Integer -> Integer -> Integer -> Integer
testExM x y n 0 = exM x y n
testExM x y n num_tests = do
	let f = exM x y n
	if num_tests /= 0 
		then (testExM x (y+1) n (num_tests-1))
		else (testExM x (y+1) n (num_tests-1))

testExpM :: Integer -> Integer -> Integer -> Integer -> Integer
testExpM x y n 0 = expM x y n
testExpM x y n num_tests = do
	let f = expM x y n
	if num_tests /= 0 
		then (testExpM x (y+1) n (num_tests-1))
		else (testExpM x (y+1) n (num_tests-1))


main1= print "haskell"
main2 = print (testExpM 20000 43000000 301 100000)
main3 = print (testExM 20000 43000000 301 100000)

----------------------------------------------------------------------------------
-- Assignment 5 (Time spent: 2 hours)
-- Run: assignment5 carmichael
-- Carmichael numbers are composite nnumbers that are made in such a way,
-- that they fool the Fermats prime test. Assignment 5 shows all the carmichael 
-- numbers that are classified as prime by the Fermat test.
-- https://en.wikipedia.org/wiki/Carmichael_number

-- From the labs.
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
    k <- [2..], 
    prime (6*k+1), 
    prime (12*k+1), 
    prime (18*k+1) ]

assignment5 :: [Integer] -> IO ()
assignment5 [] = do print "Nothing left"
assignment5 (x:xs) = do
    result <- (primeTestsF 5 x)
    if result == True
        then print x
        else return ()
    assignment5 xs

----------------------------------------------------------------------------------
-- Assignment 6 (Time spent: 30 minutes)
-- Run: assignment6 carmichael
-- The Miller-Rabin primality check performs better than the Fermats test.
-- But this test is also based on a random number generator, just like Fermats.
-- This means that occassionaly a carmichael number is classsified as prime number.
-- By increasing k, we decrease the chance that wrong random numbers are generated
-- to test for primality. With k = 5, carmichael numbers very rarely slip through.
assignment6 :: [Integer] -> IO ()
assignment6 [] = do print "Nothing left"
assignment6 (x:xs) = do
    result <- (primeMR 5 x)
    if result == True
        then print x
        else return ()
    assignment6 xs

----------------------------------------------------------------------------------
-- Assignment 7 (Time spent: 1 hour)
-- Run: assignment7 primes
-- We use the prime number generator "primes" from the lecture code.
-- We now the generated numbers are prime, so we immediately can check if
-- (2^x)-1 is prime and thus is a Mersenne number. We use the Miller-Rabin primality
-- test with k = 5. First we give the Mersenne number and then we give the according 
-- prime number.
-- https://www.mersenne.org/primes/
-- https://en.wikipedia.org/wiki/Mersenne_prime
assignment7 :: [Integer] -> IO ()
assignment7 [] = do print "Nothing left"
assignment7 (x:xs) = do
    result <- (primeMR 5 (2 ^ (x) - 1))
    if result == True
        then do
            print (2 ^ (x) - 1)
            print x
        else return ()
    assignment7 xs