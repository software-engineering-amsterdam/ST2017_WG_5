
import Lecture6
-- assignment 2 test report:
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