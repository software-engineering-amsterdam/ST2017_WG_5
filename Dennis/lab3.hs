

import Lecture3
import Data.List
import System.Random
import Test.QuickCheck
-- import Lecture3
-- 2 hours
contradiction :: Form -> Bool
contradiction x = not (satisfiable x)

tautology :: Form -> Bool
tautology x = all (\ v -> evl v x) (allVals x)

entails :: Form -> Form -> Bool
entails x y = tautology(Impl x y)

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv x y = tautology(Equiv x y)

parseHelp :: String -> Form
parseHelp s = head (parse s)

tester :: String -> Bool
tester x = if length(parse x) > 0 then show (head (parse x)) == x
            else False

--main = do
--    print ("p ^ q entails p v q")
--    print(entails (parseHelp "*(1 2)") (parseHelp "+(1 2)"))
--    print("-(p ^ q) equiv -p v -q")
--    print(equiv (parseHelp "-*(1 2)") (parseHelp "+(-1 -2)"))
--    print("contradiction -p ^ p")
--    print(contradiction(parseHelp "*(1 -1)"))
--    print("tautology p v -p")
--    print(tautology (parseHelp "+(1 -1)"))
-- precondition: No chars, only ints
main = do
    print(tester show(Equiv (Impl p q) (Impl (Neg q) (Neg p))))
    print(tester show("((1==>2)<=>(-2==>-1))"))
    print(tester "*(+(1 2) +(-2 3))")
    print(tester "+(1 -1)")
