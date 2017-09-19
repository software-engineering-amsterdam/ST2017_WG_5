

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

--assignment 2 - 2 hours
tester :: String -> Bool
tester x = if length(parse x) > 0 then show (head (parse x)) == x
            else False

--deMorgan :: Form -> Form
--deMorgan f@(Prop x) = f
--deMorgan f@(Neg (Prop x)) = f
--deMorgan (Cnj fs) = Cnj (map deMorgan fs)
--deMorgan (Dsj []) = Dsj []
--deMorgan (Dsj [f]) = deMorgan f
--deMorgan (Dsj (f:fs)) = deMorgan2 (deMorgan f) (deMorgan (Dsj fs))

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
--main = do
    --print(tester (show(Equiv (Impl p q) (Impl (Neg q) (Neg p)))))
    --print(tester (show(Equiv (Cnj[Dsj[p,q],Dsj[Neg p,r]]) (Dsj[q,r]))))
    --print(tester (show(Cnj[Dsj[p,q],Dsj[q,r],Cnj[p,r]])))
    --print(tester "*(1 -1)")
    --print(tester "")
    --print(tester "23434 * 233232")
--main = print(show(arrowfree((Impl p q)))) 

--getCharacter :: Int -> ([Char], Int)
--getCharacter a
--    | a == 1 = ("*", a)
--    | a == 2 = ("+", a)
--    | a == 3 = ("==>", a)
--    | a == 4 = ("<=>", a)

--getRandom :: IO Int
--getRandom = getStdRandom (randomR (0,4))


--getRandomHelper:: IO ()
--getRandomHelper = do
--    number <- getRandom
--    f <- getRandomOP number
--    print(f)

--getRandomOP :: Int -> IO Form
--getRandomOP x
--    | x == 0 = return (Neg x)
deMorgan :: Form -> Form
deMorgan f@(Prop x) = f
deMorgan f@(Neg (Prop x)) = f
deMorgan (Cnj fs) = Cnj (map deMorgan fs)
deMorgan (Dsj []) = Dsj []
deMorgan (Dsj [f]) = deMorgan f
deMorgan (Dsj (f:fs)) = deMorgan2 (deMorgan f) (deMorgan (Dsj fs))

deMorgan2 :: Form -> Form -> Form
deMorgan2 (Cnj []) _ = Cnj []
deMorgan2 (Cnj [f]) g  = deMorgan2 f g
deMorgan2 (Cnj (f:fs)) g = Cnj [deMorgan2 f g, deMorgan2 (Cnj fs) g]
deMorgan2  _ (Cnj []) = Cnj []
deMorgan2 f (Cnj [g]) = deMorgan2 f g
deMorgan2 f (Cnj (g:gs)) = Cnj [deMorgan2 f g, deMorgan2 f (Cnj gs)]
deMorgan2 f g = Dsj [f, g]


toCNF :: Form -> IO Form
toCNF f = 
    return (deMorgan (nnf (arrowfree f)))




getCharacter :: Int -> ([Char], Int)
getCharacter a
    | a == 1 = ("*", a)
    | a == 2 = ("+", a)
    | a == 3 = ("==>", a)
    | a == 4 = ("<=>", a)


genPropInt :: IO Int
genPropInt = getStdRandom (randomR (-10,10))

genOp :: IO Int
genOp = getStdRandom (randomR (1,4))

genCounter :: IO Int
genCounter = getStdRandom (randomR (0,3))

genProposition :: IO Form
genProposition = do
    a <- genCounter
    f <- (genPropHelper a)
    return f


genPropHelper :: Int -> IO Form
-- base case
genPropHelper 0 = do
    a <- genPropInt
    return (Prop a)

genPropHelper counter = do
    a <- genOp
    case a of 
        1 -> do
            prop1 <- (genPropHelper (counter - 1))
            prop2 <- (genPropHelper (counter - 1))
            return (Cnj [prop1, prop2])
        2 -> do
            prop1 <- (genPropHelper (counter - 1))
            prop2 <- (genPropHelper (counter - 1))
            return (Dsj [prop1, prop2])
        3 -> do
            prop1 <- (genPropHelper (counter - 1))
            prop2 <- (genPropHelper (counter - 1))
            return (Impl prop1 prop2)
        4 -> do
            prop1 <- (genPropHelper (counter - 1))
            prop2 <- (genPropHelper (counter - 1))
            return (Equiv prop1 prop2)

cnfTester ::  IO()
cnfTester = do 
    a <- genProposition
    b <- toCNF a
    let result = tautology(Equiv a b)
    if result then print("pass on: " ++ show(a) ++ " == " ++ show(b)) else print("fail on: " ++ show(a))
     



    

--randomProp :: [Char]
--randomProp = "(" ++ (show 1) ++ getOp ++ (show 3) ++ ")"
--randomProp = getOp ++ "(" ++ (show 1) ++ (show 3) ++ ")"

main = cnfTester 





