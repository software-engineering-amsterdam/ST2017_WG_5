-- Jordy Bottelier
-- Dennis Kruidenberg
-- Lars Lokhoff
-- Timo Dobber(kroket)

import Lecture3

-- Assignment 1, time spent creating code: 30 minutes. Note that we modified the lexer to work with the | and ^ operators.

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

main = do
    print ("p ^ q entails p v q")
    print(entails (parseHelp "*(1 2)") (parseHelp "+(1 2)"))
    print("-(p ^ q) equiv -p v -q")
    print(equiv (parseHelp "-*(1 2)") (parseHelp "+(-1 -2)"))
    print("contradiction -p ^ p")
    print(contradiction(parseHelp "*(1 -1)"))
    print("tautology p v -p")
    print(tautology (parseHelp "+(1 -1)"))
    
    print ("False examples:")
    print ("p ^ q not entails p ^ r")
    print(entails (parseHelp "*(1 2)") (parseHelp "^(1 3)"))
    print("-(p ^ q) not equiv -p v -r")
    print(equiv (parseHelp "-*(1 2)") (parseHelp "+(-1 -3)"))
    print("not contradiction -p ^ q")
    print(contradiction(parseHelp "*(1 -2)"))
    print("not tautology p v -q")
    print(tautology (parseHelp "+(1 -2)"))

-- Assignment 2; time spent x hours:
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
-- called on the string, created by show(), and the output is checked against the input. 
--
-- If the input format is not correct, the output should be an empty list, or an error message.
