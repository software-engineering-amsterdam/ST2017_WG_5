module Lab2 where
import Data.List
import Data.Foldable
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

-- Recognizing and generating derangements, time: 3 hours
-- Properties: 
--      Length of lists should always be the same 
--      Reflexive: isDerangement [1,2,3][3,1,2] = isDerangement [1,2,3][3,1,2]
--      Commutative: isDerangement [1,2,3][3,1,2] = isDerangement [3,1,2][1,2,3]
--      Associative: isDerangement [1,2,3][3,1,2] = isDerangement [3,1,2][1,2,3]
infix 1 //> 

(//>) :: Bool -> Bool -> Bool
p //> q = (not p) || q

length_prop :: [Int] -> [Int] -> Bool
length_prop x y = length x == length y

reflexive_prop :: [Int] -> [Int] -> Bool
reflexive_prop x y = isDerangement x y //> isDerangement y x



perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
    insrt x [] = [[x]]
    insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

isDerangement2 :: [Int] -> [Int] -> Bool
isDerangement2 [] [] = True
isDerangement2 (x:xs) (y:ys) = if x == y 
                            then False 
                            else if null xs || null ys
                            else isDerangement2 xs ys

isDerangement :: [Int] -> [Int] -> Bool
isDerangement xs ys if isPermutation xs ys
                        then isDerangement2 xs ys
                        else False

findDerans2 :: [Int] -> [[Int]] -> [[Int]] -> [[Int]]
findDerans2 original perms derans = if null perms
                                    then derans
                                    else if isDerangement original (head perms)
                                    then findDerans2 original (tail perms) (derans ++ [(head perms)])
                                    else findDerans2 original (tail perms) derans

findDerans :: [Int] -> [[Int]] -> [[Int]]
findDerans original perms = if null perms
                                    then [[0]]
                                    else if isDerangement original (head perms)
                                    then findDerans2 original (tail perms) [] ++ [(head perms)]
                                    else findDerans original (tail perms)

deran :: Int -> [[Int]]
deran x = findDerans [1..(x - 1)] (perms [1..(x - 1)])

-- ROT13
encode :: String -> String
encode [] = []
encode (x:xs) = if (ord(x) >= 65 && ord(x) <= 90)
                  then [chr ((((ord x - 65) + 13) `mod` 26) + 65)] ++ encode xs
                else if (ord(x) >= 97 && ord(x) <= 122)
                  then [chr ((((ord x - 97) + 13) `mod` 26) + 97)] ++ encode xs
                  else x:encode xs
