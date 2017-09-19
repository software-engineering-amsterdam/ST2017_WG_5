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