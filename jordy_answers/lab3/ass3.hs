import Lecture3
import Test.QuickCheck
import System.Random

-- Assignment 1, time spent: 2 hours. Note that we modified the lexer to work with the | and ^ operators.

-- a is a contradiction if it is not satisfiable
contradiction :: Form -> Bool
contradiction a = not (satisfiable a)

-- f is a tautology if every input results in True
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

-- x logically entails y if and only if the implication of x over y is a tautology
entails :: Form -> Form -> Bool
entails x y = tautology (Impl x y)

-- x is logically equivalent to y if and only if the equivalent is a tautology
equiv :: Form -> Form -> Bool
equiv x y = tautology (Equiv x y)

-- parse a string to a form, used by the formulas
parseHelp :: String -> Form
parseHelp a = head (parse a)



--main = do
--    print ("p ^ q entails p v q")
--    print(entails (parseHelp "*(1 2)") (parseHelp "+(1 2)"))
--    print("-(p ^ q) equiv -p v -q")
--    print(equiv (parseHelp "-*(1 2)") (parseHelp "+(-1 -2)"))
--    print("contradiction -p ^ p")
--    print(contradiction(parseHelp "*(1 -1)"))
--    print("tautology p v -p")
--    print(tautology (parseHelp "+(1 -1)"))
    
--    print ("False examples:")
--    print ("p ^ q not entails p ^ r")
--    print(entails (parseHelp "*(1 2)") (parseHelp "^(1 3)"))
--    print("-(p ^ q) not equiv -p v -r")
--    print(equiv (parseHelp "-*(1 2)") (parseHelp "+(-1 -3)"))
--    print("not contradiction -p ^ q")
--    print(contradiction(parseHelp "*(1 -2)"))
--    print("not tautology p v -q")
--    print(tautology (parseHelp "+(1 -2)"))


tester :: String -> Bool
tester x = if length(parse x) > 0 then show (head (parse x)) == x
            else False

--main = do
--    print "These tests should return true:"
--    print(tester (show(Equiv (Impl p q) (Impl (Neg q) (Neg p)))))
--    print(tester (show(Equiv (Impl p q) (Impl (Neg p) (Neg q)))))
--    print(tester (show(Impl (Cnj [Impl p q, Impl q r]) (Impl p r))))
--    print(tester (show (Cnj [p, q])))
--    print(tester (show(Equiv (Cnj[Dsj[p,q],Dsj[Neg p,r]]) (Dsj[q,r]))))
--    print(tester (show(Cnj[Dsj[p,q],Dsj[q,r],Cnj[p,r]])))
--    print (tester "2")

--    print "These tests should return false:"
--    print (tester "*1 2)")
--    print (tester "")
--    print (tester "*(1 2")
--    print (tester "1 => 3")
--    print (tester "(1 <=> 3 2)")
--    print (tester "*(1 <=> 3)")

-- Cases for the program:
-- 
-- Correct input format:
-- P = O1(P O2 P) | P = Digit
-- O1 = operator (+, *)
-- O2 = operator (<=>, ==>)
-- 
-- A negation can be placed anywere (if its placed behind the proposition it is skipped), the negation can only not be placed at the place of the operator.
-- O1 or O2 must be defined (not both), if it is not defined the program must error
-- If the input format is correct, the parser produces a list in which the first element is the created Form. If the parser works correctly, the output 
-- converted to a string representation should be the same as the input. To prevent ourselves from having to deal with spacing mismatches, we use a tokenized input
-- format to test the parser. When calling the show function for such an input, it creates the correct string representation for such a proposition. The parser is
-- called on the string, created by show(), and the output is checked against the input. The tokenized inputs are copied from the lecture slides
--
-- If the input format is not correct, the output should be an empty list. 
--
-- We created a tester function that compares given string input to the parser output, and we use it to test the parser functionality.
-- We created test cases with correct input format using all possible operations, to check if the output is correct when the
-- input is correct. Then we created test cases with incorrect formatting, to test if faulty input is caught.
-- The incorrect format tests included: brackets mismatched / operators on wrong places / missing operators
--
-- Results:
-- Every test case is passed, the test cases that should return true returned true, and the test cases that should return false returned false.

-- boundaries for exercise 1

                        -- Exercise CNF, time:
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
toCNF f = do 
    a <- (deMorgan (nnf (arrowfree f)))
    return a



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

cnfTester :: IO()
cnfTester = do 
    a <- genProposition
    b <- toCNF a
    print (a == b)


--randomProp :: [Char]
--randomProp = "(" ++ (show 1) ++ getOp ++ (show 3) ++ ")"
--randomProp = getOp ++ "(" ++ (show 1) ++ (show 3) ++ ")"

main = print cnfTester
--main = print (parse "(+(-7 4)<=>(2==>-8))")

