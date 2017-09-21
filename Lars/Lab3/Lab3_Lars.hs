module Lab3 where 
import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- Exercise CNF, time 8 hours:
-- For this exercise we were struggling at first to cope with the
-- the syntax and way the logic formulas were represented. After
-- figuring out how to work with them, we thought applying arrowfree
-- and nnf from the lecture3 code was sufficient to get the cnf. However, we also have to
-- apply de morgan's law to get the needed conjunction of disjunctions.
-- To get the good cnf we go through the logic formula and apply the
-- required action per conjunction or disjunction untill reaching the p,q or r.
toCNF :: Form -> Form
toCNF f = (deMorgan (nnf (arrowfree f)))

deMorgan :: Form -> Form
deMorgan (Prop f) = Prop f
deMorgan (Neg (Prop f)) = Neg (Prop f)
deMorgan (Cnj fs) = Cnj (map deMorgan fs)
deMorgan (Dsj []) = Dsj []
deMorgan (Dsj [f]) = deMorgan f
deMorgan (Dsj (f:fs)) = deMorgan2 (deMorgan f) (deMorgan (Dsj fs))

deMorgan2 :: Form -> Form -> Form
deMorgan2 (Cnj []) _ = Cnj []
deMorgan2  _ (Cnj []) = Cnj []
deMorgan2 f (Cnj [g]) = deMorgan2 f g
deMorgan2 (Cnj [f]) g  = deMorgan2 f g
deMorgan2 f (Cnj (g:gs)) = Cnj [deMorgan2 f g, deMorgan2 f (Cnj gs)]
deMorgan2 (Cnj (f:fs)) g = Cnj [deMorgan2 f g, deMorgan2 (Cnj fs) g]
deMorgan2 f g = Dsj [f, g]