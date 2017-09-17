module Lab2 where
import Data.List
import Data.Foldable
import System.Random
import Test.QuickCheck
import Data.Char

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
infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

reflexive_prop :: [Int] -> [Int] -> Bool
reflexive_prop x y = isDerangement x y --> isDerangement x y

commutative_prop :: [Int] -> [Int] -> Bool
commutative_prop x y = isDerangement x y --> isDerangement y x

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

-- ROT13, time: 4 hours
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
