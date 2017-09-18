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
start :: String -> Form
start a = head (parse a)


--main = print (contradiction (start "^(1 -1)"))
--main = print (tautology (start "|(1 -2)"))
--main = print (entails (start "|(1 2)")(start "|(1 2)"))
--main = print (entails (start "|(1 2)")(start "|(1 2)"))
main = do
	print "Hey dingen"
	print "dingen 2"