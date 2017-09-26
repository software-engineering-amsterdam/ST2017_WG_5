module Lab4 where 

import SetOrd
import Data.List
import Data.Char
import Test.QuickCheck

type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = 
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- Assignment 5 (time: 1 hour)
turnTuple :: Rel a -> Rel a
turnTuple [] = []
turnTuple (x:xs) = x : (snd x, fst x) : (turnTuple xs)

symClos :: Ord a => Rel a -> Rel a
symClos x = sort(nub (turnTuple x))

-- Assignment 6 time : 2 uur

--fixed point from the lecture code
fp :: Eq a => (a -> a) -> a -> a 
fp f = until (\ x -> x == f x) f

trClos :: Ord a => Rel a -> Rel a 
trClos a = sort(fp (\ a -> (nub) ( a ++ (a @@ a))) a)

--main = print(trClos [(1,2),(2,3),(3,4)])

-- Assignment 7
testTrClos:: (Eq a) => Ord a => Rel a  -> Bool
testTrClos a = contains a (trClos a)

testSymClos:: (Eq a) => Ord a => Rel a  -> Bool
testSymClos a = contains a (symClos a)

contains:: Eq a => [a] -> [a] -> Bool
contains a b = all (\x -> elem x b) a 

--arbitraryRels :: IO [(Int, Int)]
--arbitraryRels = do
--    k <- generate arbitrary :: IO [(Int, Int)]
--    return (k)

--retrieveNumbers :: IO [(Int, Int)] -> (Int, Int)
--retrieveNumbers k = k !! 0

genNumber :: Gen Int
genNumber = elements [0..20]

genRel :: (Gen Int, Gen Int)
genRel = (genNumber, genNumber)

newtype Rels = Rels Int deriving (Eq, Ord, Show)
instance Arbitrary Rels where
    arbitrary = genRel

--checkTrClos :: IO()
--checkTrClos = do
--    a <- arbitraryRels
--    print (a)
--    print (trClos a)
--    print (testTrClos a)

--checkSymClos :: Bool
--checkSymClos = do
--    a <- arbitraryRels
--    testSymClos a

--main = do
    --quickCheck checkSymClos

-- Assignment 8
-- The transitive closure of a the symmetric closure of a relation x and the 
-- symmetric closure of the transitive closure of a relation x are .....
-- todo: much easier with randomgenerated.....
--equalSymTr :: Rel a -> String
--equalSymTr x = if (symClos(trClos x)) == (trClos(symClos x))
--			     then "Same"
--			     else "Not same"