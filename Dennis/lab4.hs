import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Test.QuickCheck.Gen
import Test.QuickCheck.Monadic
-- time : 2 uur
type Rel a = [(a,a)]

infixr 5 @@
 
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = 
    nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

--fixed point from the lecture code
fp :: Eq a => (a -> a) -> a -> a 
fp f = until (\ x -> x == f x) f


trClos :: Ord a => Rel a -> Rel a 
trClos a= sort(fp (\ b -> (nub) ( b ++ (b @@ b))) a)

turnTuple :: Rel a -> Rel a
turnTuple [] = []
turnTuple (x:xs) = x : (snd x, fst x) : (turnTuple xs)

symClos :: Ord a => Rel a -> Rel a
symClos x = sort(nub (turnTuple x))

testTrClos:: (Eq a) => Ord a => Rel a  -> Bool
testTrClos a = contains a (trClos a)


testSymClos:: (Eq a) => Ord a => Rel a  -> Bool
testSymClos a = contains a (symClos a)


contains:: Eq a => [a] -> [a] -> Bool
contains a b = all (\x -> elem x b) a 

--https://www.stackbuilders.com/news/a-quickcheck-tutorial-generators
newtype Tester = Tester Int deriving (Eq,Show,Ord)
instance Arbitrary Tester  where 
    arbitrary = oneof $ map (return.Tester) [-10..10]


--arbitraryRels :: IO [(Int, Int)]
--arbitraryRels = do
--    k <- generate arbitrary :: IO [(Int, Int)]
--    return (k)

--checkTrClos::IO Bool
--checkTrClos = do
--    a <- arbitraryRels
--    print (a)
--    result <- testTrClos a
--    return result

checkSymClos a = testSymClos a
    where types= a::Rel Tester

checkTrClos a = testTrClos a
    where types= a::Rel Tester






main = do
    quickCheck checkSymClos
    quickCheck checkTrClos

    --print(trClos [(1,2),(2,3),(3,4)])
    --print(testTrClos [(1,2),(2,3),(3,4)])
    --print("-----")
    --print(symClos [(1,2),(2,3),(3,4)])
    --print(testSymClos [(1,2),(2,3),(3,4)])