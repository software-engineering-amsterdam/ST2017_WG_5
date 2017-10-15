module Lab6 where
import Data.List
import Test.QuickCheck
import System.Random
import Lecture6

-- Exercise 3, time: 15 minutes
composites :: [Integer]
composites = [k | 
            k <- [2..],
            not (prime k)]

-- Exercise 4, time: 30 minutes
-- k=1 : Lowest I can find is 9
-- k=2 : Also 9
-- k=3 : 65
-- By increasing k we can increase the 'correctness' of the check
composites2 :: Integer -> [Integer]
composites2 n = [k | 
            k <- [2..n],
            not (prime k)]

checkComposites :: Int ->[Integer] -> IO ()
checkComposites k (x:xs) = do
                        t <- primeTestsF k x 
                        if t
                            then if not (prime x)
                                then print ("Laagste " ++ show x ++ ".")
                                else checkComposites k xs
                        else checkComposites k xs   

