import Lecture3


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
-- P = O1(x O2 y) | P = x
-- O1 = operator (+, *)
-- O2 = operator (<=>, ==>)
-- x/y = Any digit | P
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