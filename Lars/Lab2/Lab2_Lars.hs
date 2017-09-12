module Lab2 where
import Data.List
import Data.Foldable
import Test.QuickCheck
import Test.QuickCheck.Modifiers

-- Recognizing and generating derangements
perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
    insrt x [] = [[x]]
    insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

isDerangement :: [Int] -> [Int] -> Bool
isDerangement (x:xs) (y:ys) = if x == y 
                            then False 
                            else if null xs || null ys
                                then True
                                else isDerangement xs ys

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