module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- Assignment 3: Testing properties strengths
-- From the labs
infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- From the labs.
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
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
main :: [String]
main = map snd (bubblesort [(first, "first"), (second, "second"), (third, "third"), (even, "even")])

---------------------------------------------------------------------------------------
-- Assignment 6: Implementing and testing ROT13 encoding
-- Specification??: ROT 13 is a simple letter substitution cipher, which rotates
-- the alphabet 13 places. So for example, "a" will become "n", because "n" is
-- 13 letters further in the alphabet. Because the english alphabet has 26
-- letters, this simple substitution can also be used to decode the encoded 
-- text.
-- Source: https://en.wikipedia.org/wiki/ROT13

encode :: String -> String
encode [] = []
encode (x:xs) = if (ord(x) >= 65 && ord(x) <= 90)
                  then [chr ((((ord x - 65) + 13) `mod` 26) + 65)] ++ encode xs
                else if (ord(x) >= 97 && ord(x) <= 122)
                  then [chr ((((ord x - 97) + 13) `mod` 26) + 97)] ++ encode xs
                  else x:encode xs